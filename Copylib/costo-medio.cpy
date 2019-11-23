      ***---
       CALCOLA-COSTO-MP-COMPLETO.
           perform CALCOLA-COSTO-MP.
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

      ********--- VECCHIO
      ***** CALCOLA-COSTO-MP.
      *****     if ( prg-ini-valore  of progmag    +
      *****          prg-acq-valore  of progmag    +
      *****          prg-valore-el   of progmag    +
      *****          prg-var-inv-valore of progmag -
      *****          prg-resi-fornitori-valore of progmag) = 0
      *****        move 0 to costo-mp
      *****     else
      *****        if ( prg-ini-udm     of progmag +
      *****             prg-acq-udm     of progmag +
      *****             prg-udm-el      of progmag +
      *****             prg-var-inv-udm of progmag -
      *****             prg-resi-fornitori-udm of progmag ) = 0
      *****           move 0 to costo-mp
      *****        else
      *****           compute costo-mp =
      *****                 ( prg-ini-valore     of progmag +
      *****                   prg-acq-valore     of progmag +
      *****                   prg-valore-el      of progmag +
      *****                   prg-var-inv-valore of progmag -
      *****                   prg-resi-fornitori-valore of progmag) /
      *****                 ( prg-ini-udm     of progmag +
      *****                   prg-acq-udm     of progmag +
      *****                   prg-udm-el      of progmag + 
      *****                   prg-var-inv-udm of progmag -
      *****                   prg-resi-fornitori-udm of progmag )
      *****        end-if
      *****     end-if.

      ***--- NUOVO
       CALCOLA-COSTO-MP.
           if ( prg-ini-valore  of progmag    +
                prg-acq-valore  of progmag    +
                prg-valore-el2  of progmag    +
                prg-var-inv-valore of progmag -
                prg-resi-fornitori-valore of progmag) = 0
              move 0 to costo-mp
           else
              if ( prg-ini-udm     of progmag +
                   prg-acq-udm     of progmag +
                   prg-udm-el2     of progmag +
                   prg-var-inv-udm of progmag -
                   prg-resi-fornitori-udm of progmag ) = 0
                 move 0 to costo-mp
              else
                 compute costo-mp =
                       ( prg-ini-valore     of progmag +
                         prg-acq-valore     of progmag +
                         prg-valore-el2     of progmag +
                         prg-var-inv-valore of progmag -
                         prg-resi-fornitori-valore of progmag) /
                       ( prg-ini-udm     of progmag +
                         prg-acq-udm     of progmag +
                         prg-udm-el2     of progmag + 
                         prg-var-inv-udm of progmag -
                         prg-resi-fornitori-udm of progmag )
              end-if
           end-if.
