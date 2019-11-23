      ***---
       CALCOLA-COSTO-MP-WHEN-ZERO.
           if costo-mp = 0
              move prg-costo-medio to costo-mp
              if costo-mp = 0
                 move prg-costo-ultimo to costo-mp
                 if costo-mp = 0
                    perform RECUPERO-ANAGRAFICA
                 end-if
              end-if
           end-if.

      ***---
       COPY-RECUPERO-ANAGRAFICA.
           copy "recupero-anagrafica.cpy".
