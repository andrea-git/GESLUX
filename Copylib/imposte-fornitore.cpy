      *  Si diversifica dallo standard solamente perchè si calcolano
      *  i valori delle imposte alla 4^ cifra decimale
      ***---
       CALCOLO-IMPOSTE-FORNITORE.
           move 0 to imposta-cou
                     imposta-cou-conf
                     imposta-consumo
                     imposta-consumo-conf
                     imposta-cobat
                     imposta-cobat-conf
                     add-piombo
                     add-piombo-conf.

           if art-si-imposte of articoli
              perform CALCOLO-IMPOSTA-CONSUMO
              move imposta-consumo to imposta-consumo-conf
              evaluate true
              when imf-prz-reale-utf-piu
              when imf-prz-reale-utf-meno
                   continue
              when imf-prz-reale-utf-zero
                   move 0               to imposta-consumo
              end-evaluate
                                         
              perform CALCOLO-IMPOSTA-COU
              move imposta-cou to imposta-cou-conf
              evaluate true
              when imf-prz-reale-cou-piu
              when imf-prz-reale-cou-meno
                   continue
              when imf-prz-reale-cou-zero
                   move 0           to imposta-cou
              end-evaluate
           end-if.

           if art-si-cobat of articoli
              perform CALCOLA-COBAT
              move imposta-cobat to imposta-cobat-conf
              evaluate true
              when imf-prz-reale-cobat-piu
              when imf-prz-reale-cobat-meno
                   continue
              when imf-prz-reale-cobat-zero
                   move 0             to imposta-cobat
              end-evaluate
                                        
              perform CALCOLA-ADD-PIOMBO
              move add-piombo to add-piombo-conf
              evaluate true
              when imf-prz-reale-pb-piu
              when imf-prz-reale-pb-meno
                   continue
              when imf-prz-reale-pb-zero
                   move 0 to add-piombo
              end-evaluate
           end-if.

      ***---
       CALCOLO-IMPOSTA-CONSUMO.
           move 0 to imposta-consumo.
           evaluate true
           when art-misto  of articoli
           when art-si-utf of articoli
                compute como-imposta =
               (( prg-peso-utf * imp-imposta-consumo ) 
                               * art-perce-imposte of articoli   ) / 100
           when art-no-utf of articoli
                compute como-imposta =
              (( prg-peso-non-utf * imp-imposta-consumo ) 
                                  * art-perce-imposte of articoli) / 100
           end-evaluate.
           add 0,00005       to como-imposta.
           move como-imposta to imposta-consumo.

      ***---
       CALCOLO-IMPOSTA-COU.
           move 0 to imposta-cou.
           evaluate true
           when art-misto  of articoli
           when art-si-utf of articoli
                compute como-imposta = 
                     (( prg-peso-utf * imp-cou ) 
                                     * art-perce-cou of articoli ) / 100
           when art-no-utf of articoli
                compute como-imposta =
                 (( prg-peso-non-utf * imp-cou )
                                     * art-perce-cou of articoli ) / 100
           end-evaluate.
           add 0,00005       to como-imposta.
           move como-imposta to imposta-cou.


      ***---
       CALCOLA-COBAT.
           move 0 to imposta-cobat.
           evaluate true
           when art-auto-cobat of articoli
                evaluate true
                when art-amperaggio of articoli >= imp-cb-auto-sca-1-da 
                and  art-amperaggio of articoli <= imp-cb-auto-sca-1-a
                     move imp-cb-auto-sca-1-euro 
                       to Imposta-Cobat
                when art-amperaggio of articoli >= imp-cb-auto-sca-2-da 
                and  art-amperaggio of articoli <= imp-cb-auto-sca-2-a
                     move imp-cb-auto-sca-2-euro 
                       to Imposta-Cobat
                when art-amperaggio of articoli >= imp-cb-auto-sca-3-da 
                and  art-amperaggio of articoli <= imp-cb-auto-sca-3-a
                     move imp-cb-auto-sca-3-euro 
                       to Imposta-Cobat
                when art-amperaggio of articoli >= imp-cb-auto-sca-4-da 
                and  art-amperaggio of articoli <= imp-cb-auto-sca-4-a
                     move imp-cb-auto-sca-4-euro 
                       to Imposta-Cobat
                when art-amperaggio of articoli >= imp-cb-auto-sca-5-da
                and  art-amperaggio of articoli <= imp-cb-auto-sca-5-a
                     move imp-cb-auto-sca-5-euro 
                       to Imposta-Cobat
                end-evaluate
           
           when art-moto-cobat of articoli
                evaluate true
                when art-amperaggio of articoli >= 
                     imp-cb-scooter-sca-1-da 
                 and art-amperaggio of articoli <= 
                     imp-cb-scooter-sca-1-a
                     move imp-cb-scooter-sca-1-euro 
                       to Imposta-Cobat
                when art-amperaggio of articoli >= 
                     imp-cb-scooter-sca-2-da 
                 and art-amperaggio of articoli <= 
                     imp-cb-scooter-sca-2-a
                     move imp-cb-scooter-sca-2-euro 
                       to Imposta-Cobat
                when art-amperaggio of articoli >= 
                     imp-cb-scooter-sca-3-da 
                 and art-amperaggio of articoli <= 
                     imp-cb-scooter-sca-3-a
                     move imp-cb-scooter-sca-3-euro 
                       to Imposta-Cobat
                end-evaluate
           end-evaluate.
           
           move imposta-cobat to como-imposta.
           add 0,00005        to como-imposta.
           move como-imposta  to imposta-cobat.

      ***---
       CALCOLA-ADD-PIOMBO.
           move 0 to add-piombo.
           if rlis-perce-pb not = 0
              move 0 to add-piombo
              compute risultato-imposte =
                      como-prz-unitario
              compute add-piombo-5dec   =
                      risultato-imposte -
                      risultato-imposte / (1 + (rlis-perce-pb / 100))
              add 0,00005 to add-piombo-5dec giving add-piombo
           else
              move art-marca-prodotto of articoli to tpb-marca
              move como-data-ordine               to tpb-data
              move 0 to como-prm-cliente como-prm-destino
              perform ADDIZIONALE-PIOMBO
           end-if.
