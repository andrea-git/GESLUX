      ***---
       VAL-EDI.
           set fornitore-estero to false.
           if tca-fornitore
              if cli-nazione not = "ITA"
                 set fornitore-estero to true
              end-if
           end-if.

      *    giro preventivo sulle righe del movimento per vedere se ho 
      *    dei dati da spedire e se ci sono discordanze tra il peso in 
      *    testata (manuale e modifiche ai movimenti consolidati). 
      *    In questo caso spalmo la differenza su tutte le righe
           perform CICLO-CONTRO-PESO.

           if no-edi 
              exit paragraph 
           end-if.

      *    verifico di avere la testata e la blocco
           accept como-data from century-date.

           move tmo-data-movim(1:4) to tedi-anno.
           move tmo-data-movim(5:2) to tedi-mese.

           read tedi no lock
              invalid
                 move extmovedi-user to tedi-utente-creazione
                 accept tedi-data-creazione from century-date
                 accept tedi-ora-creazione  from time
                 write tedi-rec 
                    invalid 
                       continue 
                 end-write
           end-read.

           perform READ-TEDI-LOCK.

           close tmp-redi.
           perform OPEN-IO-TMP-REDI.

      *    se non ho creato il movutf lo valorizzo per non sdoppiare il 
      *    codice dell'EDI
           if not si-movutf
              initialize mov-rec replacing numeric data by zeroes
                                      alphanumeric data by spaces
              move esercizio to mov-anno
      *    non ho il progressivo
              move 0         to mov-prog-reg
              if tca-movim-giac-pos
                 set mov-entrata to true
              else
                 set mov-uscita  to true
              end-if
              move tmo-tipo           to mov-tipo-CF
              move tmo-cod-clifor     to mov-cod-clifor
              move tmo-numdoc-clifor  to mov-num-doc
              move tmo-data-movim     to mov-data
              move 0                  to mov-num-reg
                                         mov-prog-reg
      *        accept mov-data-creazione from century-date
      *        accept mov-ora-creazione  from time
      *        move   extmovedi-user   to mov-utente-creazione
           end-if.

           set valorizza-edi to true.
      *    faccio il giro sulle righe del movimento
           perform LOOP-RIGHE-EDI.

           perform TRASFERISCI-TMP.

           move extmovedi-user  to tedi-utente-ultima-modifica.
           accept  tedi-data-ultima-modifica from century-date.
           accept  tedi-ora-ultima-modifica  from time.
           rewrite tedi-rec invalid continue end-rewrite.

           unlock tedi all records.

      ****---
       READ-TEDI-LOCK.
           set RecLocked to false.
           set tutto-ok  to true.
           read tedi with lock 
              invalid 
                 set errori to true 
           end-read.

      ***---
       VAL-REDI.
           if peso-mod and not pen-si-sdoppia-riga
              compute rmo-peso-tot-utf =  
                      rmo-peso-tot-utf + dif-peso
              if discordanza not = 0
                 add discordanza to rmo-peso-tot-utf
                 move 0 to discordanza
              end-if
           end-if.
       
           move art-cod-prodener   to pen-codice.
           read prodener
              invalid
                 initialize pen-cpa
                            pen-nc
                            pen-taric
                            pen-dac
           end-read.


           if pen-si-sdoppia-riga
              if fornitore-estero
                 perform RIGHE-3403
              end-if
           else
              if tca-movim-giac-pos
                 perform RIGHE-2710-C
              else
                 perform RIGHE-2710-S
              end-if
           end-if.

      ***---
       RIGHE-2710-C.
           if cli-nazione = "ITA"
              perform RIGHE-2710-C-ITALIA
           else
              perform RIGHE-2710-C-ESTERO
           end-if.

      ***---
       RIGHE-2710-C-ITALIA.
      *    per il carico scrivo record con causale 107 e imposta assolta
      *    senza importi

      *    valorizzo la chiave
           perform VAL-CHIAVE-REDI.

           set tmp-redi-carico   to true.
           move "107"            to tmp-redi-cau-movim.
           move pae-imp-assolta  to tmp-redi-pos-fis.
           move zero             to temp-redi-prog.

           read tmp-redi
              invalid 
                 perform INITIALIZE-TMP-REDI-DATI
           end-read.

           move pae-tipo-doc     to tmp-redi-tipo-doc.
           perform VAL-BOLLA.
           move pen-suf-reg      to tmp-redi-suf-reg.

           add  rmo-peso-tot-utf to tmp-redi-qta-kg.

           write tmp-redi-rec
              invalid 
                 rewrite tmp-redi-rec
           end-write.

      ***---
       RIGHE-2710-C-ESTERO.
      *    per il carico scrivo record con causale 107 e imposta sospesa

      *    valorizzo la chiave
           perform VAL-CHIAVE-REDI.

           set tmp-redi-carico        to true.
           move "107"                 to tmp-redi-cau-movim.
           |move pae-imp-non-soggetta       to tmp-redi-pos-fis 
quiii      move pae-imp-assolta       to tmp-redi-pos-fis.
           move zero                  to temp-redi-prog.

           read tmp-redi
              invalid 
                 perform INITIALIZE-TMP-REDI-DATI
           end-read.

           add  rmo-peso-tot-utf to tmp-redi-qta-kg.
           move rmo-peso-tot-utf to como-peso-imp.
                                           
           perform CALCOLA-IMPOSTA-CONSUMO.
           perform CALCOLA-IMPOSTA-COU.

           move pae-tipo-doc     to tmp-redi-tipo-doc.
           perform VAL-BOLLA.
           move pen-suf-reg      to tmp-redi-suf-reg.

           write tmp-redi-rec
              invalid 
                 rewrite tmp-redi-rec
           end-write.

      *    scrivo il record come scarico con causale 062 e imposta assolta
      *    non devo sommare la tasso ma solo il peso
           set tmp-redi-scarico to true.
           move "062"           to tmp-redi-cau-movim.
           move pae-imp-assolta to tmp-redi-pos-fis.
           move zero            to temp-redi-prog.

           read tmp-redi
              invalid 
                 perform INITIALIZE-TMP-REDI-DATI
           end-read.

           add  rmo-peso-tot-utf to tmp-redi-qta-kg

           move pae-tipo-doc-int to tmp-redi-tipo-doc.
           perform VAL-VIA
           move pen-suf-reg      to tmp-redi-suf-reg.

           write tmp-redi-rec
              invalid 
                 rewrite tmp-redi-rec
           end-write.

      *    scrivo il record come carico con causale 104 e imposta sospesa
      *    non devo sommare la tasso ma solo il peso
           set  tmp-redi-carico  to true.
           move "160"            to tmp-redi-cau-movim.
           move pae-imp-assolta  to tmp-redi-pos-fis.
           move 1                to temp-redi-prog.

           read tmp-redi
              invalid 
                 perform INITIALIZE-TMP-REDI-DATI
           end-read.

           add  rmo-peso-tot-utf to tmp-redi-qta-kg.

           move pae-tipo-doc-int to tmp-redi-tipo-doc.
           perform VAL-VIA
           move pen-suf-reg      to tmp-redi-suf-reg.

           write tmp-redi-rec
              invalid 
                 rewrite tmp-redi-rec
           end-write.

      ***---
       INITIALIZE-TMP-REDI-DATI.
           move zero                  to tmp-redi-qta-kg.
           move mov-num-doc           to tmo-numdoc-clifor-ed.
           call "C$JUSTIFY"     using tmo-numdoc-clifor-ed, "L".
           move tmo-numdoc-clifor-ed  to tmp-redi-mov-num-doc.
           move tmo-data-doc          to tmp-redi-mod-dt-doc.
           move space                 to tmp-redi-das.
           move cli-nazione           to naz-codice.
           read tnazioni
              invalid 
                 move space    to naz-cod-edi
           end-read.
           move naz-cod-edi           to tmp-redi-naz.
           if cli-piva not = space
              move cli-piva           to tmp-redi-dest
           else
              move cli-codfis         to tmp-redi-dest
           end-if.
           move cli-ragsoc-1          to tmp-redi-ragsoc.     
           move zero                  to tmp-redi-imp-consumo.
           move zero                  to tmp-redi-imp-cou.

           move space                 to tmp-redi-tipo-doc
                                         tmp-redi-suf-reg.

      ***---
       RIGHE-2710-S.
           if cli-nazione = "ITA"
              perform RIGHE-2710-S-ITALIA
           else
              perform RIGHE-2710-S-ESTERO
           end-if.

      ***---
       RIGHE-2710-S-ITALIA.
      *    per il carico scrivo record con causale 057 e imposta assolta
      *    non metto l'imposta

      *    valorizzo la chiave
           perform VAL-CHIAVE-REDI.

           set tmp-redi-scarico  to true.
           move "057"            to tmp-redi-cau-movim.
           move pae-imp-assolta  to tmp-redi-pos-fis.
           move zero             to temp-redi-prog.

           read tmp-redi
              invalid 
                 perform INITIALIZE-TMP-REDI-DATI
           end-read.

           add  rmo-peso-tot-utf to tmp-redi-qta-kg.

           move pae-tipo-doc     to tmp-redi-tipo-doc.
           perform VAL-BOLLA.
           move pen-suf-reg      to tmp-redi-suf-reg.

           write tmp-redi-rec
              invalid 
                 rewrite tmp-redi-rec
           end-write.

      ***---
       RIGHE-2710-S-ESTERO.
      *    per lo scarico scrivo record con causale 057 e imposta assolta
      *    senza imposta

      *    valorizzo la chiave
           perform VAL-CHIAVE-REDI.

           set  tmp-redi-scarico to true.
           move "057"            to tmp-redi-cau-movim.
           move pae-imp-assolta  to tmp-redi-pos-fis.
           move 0                to temp-redi-prog.

           read tmp-redi
              invalid 
                 perform INITIALIZE-TMP-REDI-DATI
           end-read.

           move pae-tipo-doc     to tmp-redi-tipo-doc.
           perform VAL-BOLLA.
           move pen-suf-reg      to tmp-redi-suf-reg.

           add  rmo-peso-tot-utf to tmp-redi-qta-kg.

           write tmp-redi-rec
              invalid 
                 rewrite tmp-redi-rec
           end-write.

      ***---
       RIGHE-3403.
      *    scrivo il primo carico con causale 161 e imposta assolta

      *    valorizzo la chiave
           perform VAL-CHIAVE-REDI.

           set  tmp-redi-carico      to true.
           move "161"                to tmp-redi-cau-movim.
      *     move pae-imp-assolta to tmp-redi-pos-fis 
           move pae-imp-non-soggetta to tmp-redi-pos-fis.
           move 0                    to temp-redi-prog.

           read tmp-redi
              invalid 
                 perform INITIALIZE-TMP-REDI-DATI
           end-read.

           compute como-peso = rmo-peso-tot - 
                             ( rmo-peso-tot / 100 * 
                             ( art-perce-imposte + art-perce-cou) ).

           add como-peso     to tmp-redi-qta-kg.
              
           move pae-tipo-doc to tmp-redi-tipo-doc.
           perform VAL-BOLLA.
           move pen-suf-reg  to tmp-redi-suf-reg.

           write tmp-redi-rec
              invalid 
                 rewrite tmp-redi-rec
           end-write.

      *    scrivo il record di carico con causale 161 e imposta sospesa
           move "161"                 to tmp-redi-cau-movim.
quii  *     move pae-imp-non-soggetta       to tmp-redi-pos-fis 
           move pae-imp-assolta       to tmp-redi-pos-fis.
           move 0                     to temp-redi-prog.

           read tmp-redi
              invalid 
                 perform INITIALIZE-TMP-REDI-DATI
           end-read.

           compute como-peso = rmo-peso-tot - como-peso.

           add como-peso      to tmp-redi-qta-kg.

           move rmo-peso-tot to como-peso-imp.
           perform CALCOLA-IMPOSTA-CONSUMO.
           perform CALCOLA-IMPOSTA-COU.

           move pae-tipo-doc          to tmp-redi-tipo-doc.
           perform VAL-BOLLA.
           move pen-suf-reg           to tmp-redi-suf-reg.

           write tmp-redi-rec
              invalid 
                 rewrite tmp-redi-rec
           end-write.

      *    scrivo il record di scarico con causale 062 e imposta assolta
           set tmp-redi-scarico to true.
           move "062"           to tmp-redi-cau-movim.
           move pae-imp-assolta to tmp-redi-pos-fis.
           move zero            to temp-redi-prog.

           read tmp-redi
              invalid 
                 perform INITIALIZE-TMP-REDI-DATI
           end-read.

           add rmo-peso-tot  to tmp-redi-qta-kg.

           move pae-tipo-doc-int      to tmp-redi-tipo-doc.
           perform VAL-VIA
           move pen-suf-reg           to tmp-redi-suf-reg.

           write tmp-redi-rec
              invalid 
                 rewrite tmp-redi-rec
           end-write.

      ***---
       CICLO-CONTRO-PESO.
           move zero            to num-righe.
           set no-edi           to true.
           set controllo-peso   to true.

           set peso-mod   to false.
           move zero      to peso-calcolato.

           perform LOOP-RIGHE-EDI.

           if si-edi
              if si-movutf and tca-movim-giac-neg
                 move mov-kg    to peso-calcolato
              end-if
      *
              if peso-calcolato not = tmo-peso-utf
                 set peso-mod   to true

                 compute dif-peso = ( tmo-peso-utf - peso-calcolato ) /
                                      num-righe

                 |Nel caso di arrotondamento errato metto
                 |la discordanza sulla PRIMA RIGA che trovo (che non 
                 |sarà per forza la prima in visualizzazione nel pgm
                 |relativo risultando quindi un aumento su una riga 
                 |casuale)
                 |Es. 0,162 su 7 righe
                 compute discordanza =  
                      ( tmo-peso-utf - peso-calcolato ) - 
                      ( num-righe * dif-peso )
              end-if
           end-if.

      ***---
       LOOP-RIGHE-EDI.
           set trasferisci  to false
      *    faccio il giro sulle righe del movimento
           set  tutto-ok   to true
           move tmo-anno   to rmo-anno
           move tmo-numero to rmo-movim
           move low-value  to rmo-riga
           start rmovmag   key >= rmo-chiave
                 invalid   set errori to true
           end-start
           if tutto-ok
              perform until 1 = 2
                 set  record-ok  to false
                 read rmovmag next at end exit perform end-read
                 if rmo-anno  not = tmo-anno or 
                    rmo-movim not = tmo-numero
                    exit perform
                 end-if
                 move rmo-codmag  to mag-codice
                 read tmagaz no lock 
                      invalid   continue
                  not invalid
                      if mag-si-utf
                         set record-ok to true
                      end-if
                 end-read
                 if record-ok
                    set record-ok     to false
                    move rmo-articolo to art-codice
                    read articoli  no lock
                         invalid continue
                     not invalid
                         move art-cod-prodener   to pen-codice
                         read prodener no  lock
                              invalid  set record-ok to false
                          not invalid
                              if pen-si-invio-edi
                                 set record-ok to true
                              end-if
                         end-read 
                    end-read
                 end-if
                 if record-ok
                    perform CONTROLLO-TRASFERISCI
LUBEXX              if trasferisci
                       if controllo-3403
                          exit perform
                       end-if
                       perform LOOP-RIGHE-EDI-EXE
                    end-if
LUBEXX           end-if
              end-perform
           end-if.

      ***---
       CONTROLLO-TRASFERISCI.
           set trasferisci to false.

           if pen-si-sdoppia-riga
      *    se 3403 trasferisco la riga solo per
      *    i carichi e se fornitore estero
              if fornitore-estero
                 if art-perce-imposte not = 0 or art-perce-cou not = 0
                    set trasferisci to true
                 end-if
              end-if
           else
              if rmo-peso-tot-utf not = 0
                 set trasferisci to true
              end-if
           end-if.

      *    vecchio codice       
LUBEXX*     if rmo-peso-tot-utf = 0
      *        if pen-si-sdoppia-riga and fornitore-estero
      *           if art-perce-imposte not = zero
      *              set trasferisci to true
      *           end-if
      *        end-if
      *     else
      *        set trasferisci to true
      *     end-if.


      ***---
       LOOP-RIGHE-EDI-EXE.
           evaluate true
           when controllo-peso
                set si-edi   to true
                compute peso-calcolato = peso-calcolato + 
                                         rmo-peso-tot-utf
                move rmo-chiave to como-rmo-chiave       
      *    per il peso da spalmare devo prendere in considerazione solo
      *    le righe che non sdoppiano
                if not pen-si-sdoppia-riga
                   add 1           to num-righe
                end-if
           when valorizza-edi
                perform VAL-REDI
           end-evaluate.

      ***---
       TRASFERISCI-TMP.
           move tedi-chiave to redi-tedi-chiave.
           move high-value  to redi-prog.
           start redi key not > redi-chiave
                 invalid move 0 to redi-prog
             not invalid
                 read REDI previous no lock
                      at end move 0 to redi-prog
                  not at end
                      if tedi-chiave not = redi-tedi-chiave
                         move 0 to redi-prog
                      end-if
                 end-read
           end-start.
           move tedi-chiave  to redi-tedi-chiave.
           
           move low-value to tmp-redi-chiave.
           start tmp-redi key not < tmp-redi-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2 
                    read tmp-redi next at end exit perform end-read
                    perform TRASFERISCI-TMP-REDI
                 end-perform
           end-start.

      ***---
       TRASFERISCI-TMP-REDI.
           set trovato to true.

           add 1                         to redi-prog.

           move tmp-redi-tipo-movim      to redi-tipo-movim.
           move tmp-redi-chiave-mov      to redi-chiave-mov.
           move tmp-redi-codi-prodotto   to redi-codi-prodotto.
           move tmp-redi-tipo-stoc       to redi-tipo-stoc.
           move tmp-redi-qta-kg          to redi-qta-kg.
           move tmp-redi-mov-num-doc     to redi-mov-num-doc.
           move tmp-redi-mod-dt-doc      to redi-mod-dt-doc.
           move tmp-redi-das             to redi-das.
           move tmp-redi-naz             to redi-naz.
           move tmp-redi-dest            to redi-dest.
           move tmp-redi-cau-movim       to redi-cau-movim.
           move tmp-redi-pos-fis         to redi-pos-fis.    
           move tmp-redi-imp-consumo     to redi-imp-consumo.
           move tmp-redi-imp-cou         to redi-imp-cou.
           move tmp-redi-ragsoc          to redi-ragsoc.
      *     move tmp-redi-stato           to redi-stato
           move tmp-redi-tmo-chiave      to redi-tmo-chiave.
           move tmp-redi-data-movimento  to redi-data-movimento.

           move tmp-redi-tipo-doc        to redi-tipo-doc.
           perform VAL-BOLLA.
           move tmp-redi-suf-reg         to redi-suf-reg.

           set redi-inserito to true.

           move tedi-dati-comuni         to redi-dati-comuni
           write redi-rec invalid rewrite redi-rec end-write.

           add 1 to extmovedi-tot-EDI.

      ***---
       VAL-CHIAVE-REDI.
           move tedi-chiave     to tmp-redi-tedi-chiave.
           move mov-anno        to tmp-redi-mov-anno
      *                             utf-anno.
           move mov-num-reg     to tmp-redi-num-reg.
           move mov-prog-reg    to tmp-redi-mov-prog-reg.
           move tmo-data-movim  to tmp-redi-data-movimento.
           move pen-cpa         to tmp-redi-cpa.
           move pen-nc          to tmp-redi-nc.
           move pen-taric       to tmp-redi-taric.
           move pen-dac         to tmp-redi-dac.

           evaluate true
           when art-confezionato
                move pae-tipo-conf    to tmp-redi-tipo-stoc
           when art-sfuso
                move pae-tipo-sfuso   to tmp-redi-tipo-stoc
           end-evaluate.

           move tmo-chiave            to tmp-redi-tmo-chiave.
                               
      ***---
       CALCOLA-IMPOSTA-CONSUMO.
           |03/08/09 richiesta di Walter: gli articoli 2710 con 
           |marca FIAT (11) devono essere sommati al 100%
           if art-cod-doganale = 27100000 and art-marca-prodotto = 11
              compute como-imposta = como-peso-imp * imp-imposta-consumo
           else
              compute como-imposta = ( como-peso-imp             *
                                       art-perce-imposte / 100 ) *
                                      imp-imposta-consumo
           end-if.
           add 0,005               to como-imposta.
           move como-imposta       to como-imposta-2dec.
           add como-imposta-2dec   to tmp-redi-imp-consumo.


      *     compute como-imposta = (como-peso-imp * art-perce-cou / 100) 
      *                             * imp-cou
      *
      *     add 0,005               to como-imposta             
      *     move como-imposta       to como-imposta-2dec
      *     add como-imposta-2dec   to tmp-redi-imp-consumo.
      *

      ***---
       CALCOLA-IMPOSTA-COU.
           |03/08/09 richiesta di Walter: gli articoli 2710 con 
           |marca FIAT (11) devono essere sommati al 100%
           if art-cod-doganale = 27100000 and art-marca-prodotto = 11
              compute como-imposta = como-peso-imp * imp-cou
           else
              compute como-imposta = ( como-peso-imp         *
                                       art-perce-cou / 100 ) *
                                       imp-cou
           end-if.
           add 0,005               to como-imposta.
           move como-imposta       to como-imposta-2dec.
           add como-imposta-2dec   to tmp-redi-imp-cou.


      *     compute como-imposta = (como-peso-imp * art-perce-cou / 100) 
      *                             * imp-cou
      *
      *     add 0,005               to como-imposta             
      *     move como-imposta       to como-imposta-2dec
      *     add como-imposta-2dec   to tmp-redi-imp-consumo.
      *

      ***---
       VAL-VIA.
           move tmo-num-via           to tmo-numdoc-clifor-ed.
           call "C$JUSTIFY"        using tmo-numdoc-clifor-ed, "L".
           move tmo-numdoc-clifor-ed  to tmp-redi-mov-num-doc.
           move tmo-dt-via            to tmp-redi-mod-dt-doc.

      ***---
       VAL-BOLLA.
           move mov-num-doc           to tmo-numdoc-clifor-ed.
           call "C$JUSTIFY"        using tmo-numdoc-clifor-ed, "L".
           move tmo-numdoc-clifor-ed  to tmp-redi-mov-num-doc.
           move tmo-data-doc          to tmp-redi-mod-dt-doc.

      ***---
       LOOP-RIGHE-CONTROLLO.
      *    faccio il giro sulle righe del movimento
           set  tutto-ok   to true
           move tmo-anno   to rmo-anno
           move tmo-numero to rmo-movim
           move low-value  to rmo-riga
           start rmovmag   key >= rmo-chiave
                 invalid   set errori to true
           end-start
           if tutto-ok
              perform until 1 = 2
                 set  record-ok  to false
                 read rmovmag  next 
                    at end 
                       exit perform 
                 end-read
                 if rmo-anno  not = tmo-anno or 
                    rmo-movim not = tmo-numero
                    exit perform
                 end-if
                 move rmo-codmag  to mag-codice
                 read tmagaz  no lock 
                      invalid continue
                  not invalid
                      if mag-si-utf
                         set record-ok to true
                      end-if
                 end-read
                 if record-ok
                    set record-ok     to false
                    move rmo-articolo to art-codice
                    read articoli no lock
                         invalid  continue
                     not invalid
                         if not art-no-utf
                            move art-cod-prodener   to pen-codice
                            read prodener  no  lock
                                 invalid perform VAL-ARTICOLI
                            end-read
                         end-if
                    end-read
                 end-if
              end-perform
           end-if.
      
      ***---
       VAL-ARTICOLI.
           set problemi-articoli   to true.

           move art-codice         to tmp-anp-codice
           move art-descrizione    to tmp-anp-descr
           if art-cod-prodener = space
              move "Prodotto energetico non valorizzato"   
                                                  to tmp-anp-problema
           else
              initialize tmp-anp-problema
              string "Cod. "                       delimited size
                     art-cod-prodener              delimited size
                     " non presente in anagrafica" delimited size
                into tmp-anp-problema
           end-if

           write tmp-anp-rec invalid continue end-write.
                     

      ***---
       CREA-REPORT.
           set extmovedi-problemi-art to true
           accept  wstampa from environment "PATH_ST".
           inspect wstampa replacing trailing space by low-value

           string wstampa           delimited low-value
                  "art_no_prodener" delimited size
                  como-data         delimited size
                  "_"               delimited size
                  como-ora          delimited size
                  ".txt"            delimited size
                  into wstampa
           inspect wstampa  replacing trailing low-value by space.
           move wstampa   to extmovedi-path-no-art

           open output LINESEQ.

           perform SCRIVI-TESTA-REPORT

           move low-value to tmp-anp-codice.
           start TMP-ART-NO-PRODENER key not < tmp-anp-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read TMP-ART-NO-PRODENER next
                       at end exit perform
                    end-read
                    perform SCRIVI-RIGA-REPORT
                 end-perform
           end-start.
           close LINESEQ.

      ***---
       SCRIVI-TESTA-REPORT.
           move "Impossibile generare i movimenti UFT telematici."
                          to line-riga.
           write LINE-RIGA.
           move      "Alcuni articoli riportano problemi con i prodotti 
      -     "energetici"  to line-riga.
           write LINE-RIGA.

           move all "-"   to line-riga(1:100).
           write LINE-RIGA after 2.

           move "Articolo"      to rr-codice.
           move "Descrizione"   to rr-descr.
           move "Problema"      to rr-problema.

           write LINE-RIGA from riga-report.
           move all "-"   to line-riga(1:100).
           write LINE-RIGA.  

      ***---
       SCRIVI-RIGA-REPORT.
           move tmp-anp-codice     to art-ed.
           move art-ed             to rr-codice.  
           move tmp-anp-descr      to rr-descr.   
           move tmp-anp-problema   to rr-problema.

           write LINE-RIGA from riga-report.

      ***---
       VERIFICA-MOVIMENTO.
           set fornitore-estero to false.
           if tca-fornitore
              if cli-nazione not = "ITA"
                 set fornitore-estero to true
              end-if
           end-if.

           set controllo-3403 to true.
           perform LOOP-RIGHE-EDI.
           if not trasferisci
              set errori  to true
           end-if.
