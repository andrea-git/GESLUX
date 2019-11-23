      ***---
       CARICA-GRID.
           perform RESET-GRIGLIA.

           modify gd-fatt, mass-update = 1.
           move low-value  to ror-rec.
           move tor-chiave to ror-chiave.
           start rordini key is >= ror-chiave
                 invalid continue
             not invalid
                 move 2 to riga
                 perform until 1 = 2
                    read rordini next at end exit perform end-read
                    if ror-anno       not = tor-anno or
                       ror-num-ordine not = tor-numero
                       exit perform
                    end-if
                    perform AGGIUNGI-RIGA
OMAGGI              if ror-qta-omaggi not = 0
OMAGGI                 move ror-qta-omaggi   to ror-qta
OMAGGI                 move 0                to ror-prz-unitario
OMAGGI                 move 0                to ror-perce-sconto
OMAGGI                 move 0                to ror-imp-consumo
OMAGGI                 move 0                to ror-imp-cou-cobat
OMAGGI                 move 0                to ror-imponib-merce
OMAGGI                 move tge-cod-iva-omag to ror-cod-iva
OMAGGI                 set  ror-si-omaggio   to true
OMAGGI                 perform AGGIUNGI-RIGA
OMAGGI              end-if
                 end-perform
           end-start.
           modify gd-fatt, mass-update = 0.
