      * Tenere allineato con imposte-fornitore.cpy
      ***---
       CALCOLA-IMPOSTE.
           move 0 to risultato-imposte
                     imposta-cou
                     imposta-cobat
                     imposta-consumo
                     TipoImposta.

           move art-marca-prodotto to mar-codice.
           read tmarche no lock 
                invalid 
                set mar-no-cou   to true
                set mar-no-cobat to true
           end-read.

           if art-si-cobat
              if not TrattamentoGDO
                 if mar-si-cobat
                    perform CALC-COBAT
                 end-if
              else
                 perform CALC-COBAT
              end-if
           end-if.

           if art-si-imposte
              if not TrattamentoGDO
                 if mar-si-imposta-consumo
                    perform CALC-CONSUMO
                 end-if
                 if mar-si-cou
                    perform CALC-COU
                 end-if
              else
                 |NON FACCIO IL TEST SU MARCA!!!
                 perform CALC-CONSUMO
                 perform CALC-COU
              end-if

              if ImpostaCobat set ImpostaCouCobat to true
              else            set ImpostaCou      to true
              end-if

           end-if.

      ***---
       CALC-COBAT.
           set ImpostaCobat to true.
           evaluate true
           when art-auto-cobat
                evaluate true
                when   art-amperaggio >= imp-cb-auto-sca-1-da and
                       art-amperaggio <= imp-cb-auto-sca-1-a
                       move imp-cb-auto-sca-1-euro
                         to risultato-imposte
                when   art-amperaggio >= imp-cb-auto-sca-2-da and
                       art-amperaggio <= imp-cb-auto-sca-2-a
                       move imp-cb-auto-sca-2-euro
                         to risultato-imposte
                when   art-amperaggio >= imp-cb-auto-sca-3-da and
                       art-amperaggio <= imp-cb-auto-sca-3-a
                       move imp-cb-auto-sca-3-euro
                         to risultato-imposte
                when   art-amperaggio >= imp-cb-auto-sca-4-da and
                       art-amperaggio <= imp-cb-auto-sca-4-a
                       move imp-cb-auto-sca-4-euro
                         to risultato-imposte
                when   art-amperaggio >= imp-cb-auto-sca-5-da and
                       art-amperaggio <= imp-cb-auto-sca-5-a
                       move imp-cb-auto-sca-5-euro
                         to risultato-imposte
                end-evaluate

           when art-moto-cobat
                evaluate true
                when art-amperaggio >= imp-cb-scooter-sca-1-da and
                     art-amperaggio <= imp-cb-scooter-sca-1-a
                     move imp-cb-scooter-sca-1-euro
                       to risultato-imposte
                when art-amperaggio >= imp-cb-scooter-sca-2-da and
                     art-amperaggio <= imp-cb-scooter-sca-2-a
                     move imp-cb-scooter-sca-2-euro
                       to risultato-imposte
                when art-amperaggio >= imp-cb-scooter-sca-3-da and
                     art-amperaggio <= imp-cb-scooter-sca-3-a
                     move imp-cb-scooter-sca-3-euro
                       to risultato-imposte
                end-evaluate
           end-evaluate.

           add 0,005              to risultato-imposte.
           move risultato-imposte to imposta-cobat.
                                                  
      ***---
       CALC-CONSUMO.
           compute risultato-imposte   = |--> ror-imp-consumo
           ( ( ( prg-peso-utf     of progmag + 
                 prg-peso-non-utf of progmag ) 
              * imp-imposta-consumo ) 
              * art-perce-imposte   ) / 100.
           add 0,005               to risultato-imposte.
           move risultato-imposte  to imposta-consumo.

      ***---
       CALC-COU.
           compute risultato-imposte = |--> ror-imp-cou-cobat
           ( ( ( prg-peso-utf     of progmag + 
                 prg-peso-non-utf of progmag ) 
                 * imp-cou ) 
                 * art-perce-cou ) / 100.
           add 0,005               to risultato-imposte.
           move risultato-imposte  to imposta-cou.
