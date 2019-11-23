      ***---
       MARGINE-RICALCOLATO.
           move low-value to prr-rec.
           start progmagric key >= prr-chiave
                 invalid continue 
           end-start.
           perform until 1 = 2
              read progmagric next at end exit perform end-read

              if prr-peso = 0
                 initialize art-rec replacing numeric data by zeroes
                                         alphanumeric data by spaces
                 move prr-cod-articolo to art-codice
                 read articoli no lock invalid continue end-read

      *****           if prr-costo-medio = 0
      *****              move prr-prz-anagrafica to prr-costo-medio
      *****           end-if

                 if prr-costo-medio = 0
                    if prr-acq-udm = 0 and prr-ini-udm = 0
                       move prr-costo-ultimo to prr-costo-medio
                       if prr-costo-medio = 0
                          move prr-costo-inizio to prr-costo-medio
                          if prr-costo-medio = 0
                             move prr-prz-anagrafica to prr-costo-medio
                          end-if
                       end-if
                    end-if
                 end-if

                 perform CALCOLA-VALORI
                 perform SOMMA-VALORI
              end-if

           end-perform.

      ***---
       MARGINE-CONSOLIDATO.
           |CALCOLO I TOTALI 
           |FACCIO LE SOMMATORIE PER MARCA
           move low-value to sts-chiave of statsett.
           move link-mese to sts-mese   of statsett.
           start statsett key >= k-ord
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read statsett next at end   exit perform end-read
                    if sts-mese  of statsett not = link-mese 
                       exit perform 
                    end-if
                    initialize tmdm-rec replacing numeric data by zeroes
                                             alphanumeric data by spaces
                    move sts-marca of statsett to tmdm-marca
                    read tmp-delta-marca invalid continue end-read

                    |I valori prog vanno presi dal mese precedente
                    move 0 to sts-fat-prog of statsett2
                    move 0 to sts-csm-prog of statsett2
                    if link-mese not = 1
                       move sts-chiave of statsett to
                            sts-chiave of statsett2
                       subtract 1 from sts-mese of statsett2
                       read statsett2 no lock invalid continue end-read
                    end-if

                    |Come da richiesta di Mori 16/01/08:
                    |Prendo il margine cumulato + la parte in corso
      *****              compute margine = sts-fat-corr - 
      *****                                sts-csm-corr
      *****                                |NON TENGO CONTO DELL'ADEGUAMENTO
      *****                                |IN QUANTO NON ANCORA CALCOLATO
      *****                                |sts-adeguam-corr
                    compute margine = ( sts-fat-corr of statsett + 
                                        sts-fat-prog of statsett2 ) -
                                      ( sts-csm-corr of statsett + 
                                        sts-csm-prog of statsett2 )
      *****              add margine to tmdm-marg-statsett
                    |Richiesta di Mori (30/01/08): IN VALORE ASSOLUTO
                    move margine    to margine-pos
                    add margine-pos to tmdm-marg-statsett-pos

                    |mi serve la sommatoria della resa complessiva
                    |MENSILE PER MARCA da utilizzare dopo
                    compute margine = sts-fat-corr of statsett - 
                                      sts-csm-corr of statsett

                    add margine to tmdm-marg-mese
                    write tmdm-rec invalid rewrite tmdm-rec end-write

                 end-perform
           end-start.

      ***---
       CALCOLA-VALORI.
           compute como-vendite =
                   prr-ven-valore - prr-resi-da-cli-valore.

           compute como-acquisti =
                   prr-acq-valore - prr-resi-fornitori-valore +
                   prr-valore-el  - prr-valore-ul             +
                   prr-var-inv-valore.

           compute finali  = prr-giacenza-udm * prr-costo-medio.

           compute margine = como-vendite   +
                             finali         -
                             prr-ini-valore -
                             como-acquisti.
      ***---
       SOMMA-VALORI.
           initialize tmdm-rec replacing numeric data by zeroes
                                   alphanumeric data by spaces.
           move art-marca-prodotto to tmdm-marca.
           read tmp-delta-marca invalid continue end-read.
           add  margine   to tmdm-marg-calc.
           write tmdm-rec  invalid rewrite tmdm-rec end-write.

      ***---
       CALCOLO-DELTA.
           |Prendo il valore (resa) per tipologia cliente
           |e ne calcolo la % sulla resa
           initialize tmdm-rec replacing numeric data by zeroes
                                    alphanumeric data by spaces.
           move sts-marca of statsett to tmdm-marca mar-codice.
           read tmarche no lock invalid continue end-read.
           read tmp-delta-marca invalid continue end-read.
           |CALCOLO COLONNE
           |1. Margine statistiche
           |**************************************************
           |*       Presente sul file delle statistiche      *
           |**************************************************
           |Come da richiesta di Mori 16/01/08:
           |Prendo il margine globale
      *****     compute margine = sts-fat-corr -
      *****                       sts-csm-corr.
      *****                       |NON TENGO CONTO DELL'ADEGUAMENTO
      *****                       |IN QUANTO NON ANCORA CALCOLATO
      *****                       |sts-adeguam-corr

           |I valori prog vanno presi dal mese precedente
           move 0 to sts-fat-prog of statsett2.
           move 0 to sts-csm-prog of statsett2.
           if link-mese not = 1
              move sts-chiave of statsett to
                   sts-chiave of statsett2
              subtract 1 from sts-mese of statsett2
              read statsett2 no lock invalid continue end-read
           end-if.

           compute margine = ( sts-fat-prog of statsett2  +
                               sts-fat-corr of statsett ) -
                             ( sts-csm-prog of statsett2  +
                               sts-csm-corr of statsett ).

           |% d'incidenza della resa settore
           |su resa statistica globale
           |**************************************************
           |*     INCIDENZA = RESA (GDO, Tradizionale,       *
           |* modulo valore +, ecc.) x 100 / RESA STATISTICA *
           |**************************************************

           if link-mese = 1
              move 0 to str-vag-prog
           else
              subtract 1 from link-mese
                       giving  str-mese
              move mar-codice to str-marca
              read statraff   no lock 
                   invalid  move 0 to str-vag-prog
              end-read
           end-if.

           |Richiesta di Mori (30/01/08): usare il margine in valore
           |assoluto. Al campo "tmdm-marg-statsett-pos" sono già stati
           |applicate le somme in valore assoluto

           move margine to margine-pos.

           compute incidenza rounded =
                   margine-pos * 100 / tmdm-marg-statsett-pos.

           |2. Calcolo del "delta" (calcolata per ogni marca 
           |e spaccata per settore in base % incidenza)
           |**************************************************
           |* DELTA (calcolata per ogni marca e spaccata per *
           |* settore in base % incidenza) = (MARGINE RICAL  *
           |* - MARGINE CONSOLIDATO) x INCIDENZA ) / 100)    *
           |**************************************************

           compute delta =
                   tmdm-marg-calc -
                   str-vag-prog   -
                   tmdm-marg-mese.

           compute delta rounded =
                   delta * incidenza / 100.

           compute delta = ( sts-fat-corr of statsett -
                             sts-csm-corr of statsett ) + delta.
