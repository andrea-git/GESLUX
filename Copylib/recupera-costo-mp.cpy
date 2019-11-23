      ***---
       RECUPERA-COSTO-MP.
           perform CALCOLA-COSTO-MP.
           if prg-costo-ultimo = 0 and costo-mp = 0
             |Se ho fatto degli acquisti vuol dire che l'articolo è stato
             |caricato con prezzo 0 perciò devo tenere 0 come valore,
             |altrimenti andando a prendere il prezzo in anagrafica è come
             |se caricassi a costo un prodotto che mi è stato regalato
              if prg-acq-udm  not = 0
                 move 0 to costo-mp
              else
                 if prg-costo-medio not = 0
                    move prg-costo-medio to costo-mp
                 else
                    perform RECUPERO-ANAGRAFICA
                 end-if
              end-if
           else
              |Prendo il maggiore
              if prg-costo-ultimo > costo-mp
                 move prg-costo-ultimo to costo-mp
              end-if
           end-if.
