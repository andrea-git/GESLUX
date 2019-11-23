      ***---
       CALCOLA-COSTO-MP-COMPLETO.
           move prg-acq-udm of progmag to save-acq-udm.
           move prg-ini-udm of progmag to save-ini-udm.
           perform CALCOLA-COSTO-MP.
           if forza-giro-completo
              move 0 to prg-acq-udm of progmag
              move 0 to prg-ini-udm of progmag
           end-if.
           if costo-mp = 0
              if prg-acq-udm of progmag = 0 |VERIFICO ACQUISTI
                 if prg-ini-udm of progmag = 0 |VERIFICO RIMANENZE INIZIALI
                    if prg-costo-ultimo      of progmag not = 0
                       move prg-costo-ultimo of progmag to costo-mp
                       set recupero-ultimo   to true
                    else
                       if prg-costo-medio of progmag not = 0
                          move prg-costo-medio  of progmag to costo-mp
                          set recupero-iniziale to true
                       else
                          perform RECUPERO-ANAGRAFICA
                          set recupero-anagrafica to true
                       end-if
                    end-if
                 else
                    set recupero-ini to true
                 end-if
              else
                 set recupero-acq to true
              end-if
           else
              set recupero-normale to true
           end-if.
           move save-acq-udm to prg-acq-udm of progmag.
           move save-ini-udm to prg-ini-udm of progmag.

      ***---
           |Richiesta del 21/05/2008: non considerare le entrate di lavorazione
       CALCOLA-COSTO-MP.
           set forza-giro-completo to false.
           if ( prg-ini-valore            of progmag +
                prg-acq-valore            of progmag +
      *****          prg-valore-el             of progmag +
                prg-var-inv-valore        of progmag +
                prg-resi-fornitori-valore of progmag) = 0
              move 0 to costo-mp
           else
              if ( prg-ini-udm     of progmag +
                   prg-acq-udm     of progmag +
      *****             prg-udm-el      of progmag +
                   prg-var-inv-udm of progmag +
                   prg-resi-fornitori-udm of progmag ) = 0
                 move 0 to costo-mp
              else
                 |Forzo a zero i valori di controllo delle iniziali
                 |e degli acquisti per avere il giro completo
                 if ( prg-ini-udm     of progmag +
                      prg-acq-udm     of progmag +
      *****                prg-udm-el      of progmag +
                      prg-var-inv-udm of progmag -
                      prg-resi-fornitori-udm of progmag ) = 0
                    move 0 to costo-mp
                    set forza-giro-completo to true
                 else
                    compute costo-mp =
                          ( prg-ini-valore of progmag +
                            prg-acq-valore of progmag +
      *****                      prg-valore-el  of progmag +
                            prg-var-inv-valore of progmag -
                            prg-resi-fornitori-valore of progmag) /
                          ( prg-ini-udm of progmag     +
                            prg-acq-udm of progmag     +
      *****                      prg-udm-el  of progmag     +
                            prg-var-inv-udm of progmag -
                            prg-resi-fornitori-udm of progmag )
                 end-if
              end-if
           end-if.
