      ***---
       TROVA-ORDINE-NOTA.
           set  trovato-ordine  to false.
           set  trovato-nota    to false.
           move tmo-causale     to tca-codice.
           read tcaumag no lock invalid continue end-read.
           if tca-nota-credito-si
              move tmo-anno-fattura to tno-anno-fattura
              move tmo-num-fattura  to tno-num-fattura
              read tnotacr no lock key k-fattura
                   invalid continue
               not invalid
                   if tno-causale      = tmo-causale       and
                      tno-cod-cli      = tmo-cod-clifor    and
                      tno-num-fattura  = tmo-numdoc-clifor and
                      tno-data-fattura = tmo-data-doc
                      set trovato-nota to true
                   end-if
              end-read
           else
              move tmo-anno-fattura to tor-anno-fattura
              move tmo-num-fattura  to tor-num-fattura
              read tordini no lock key k-fattura
                   invalid 
                   move tmo-anno-fattura to tno-anno-fattura
                   move tmo-num-fattura  to tno-num-fattura
                   read tnotacr no lock key k-fattura
                        invalid continue
                    not invalid
                        if tno-causale      = tmo-causale       and
                           tno-cod-cli      = tmo-cod-clifor    and
                           tno-num-fattura  = tmo-numdoc-clifor and
                           tno-data-fattura = tmo-data-doc
                           set trovato-nota to true
                        end-if
                   end-read
               not invalid
                   if tor-causale    = tmo-causale       and
                      tor-cod-cli    = tmo-cod-clifor    and
                      tor-num-bolla  = tmo-numdoc-clifor and
                      tor-data-bolla = tmo-data-doc
                      set trovato-ordine to true
                   end-if
              end-read
           end-if.

      ***---
       TROVA-ADDIZIONALE-ORDINE.
           move 0      to add-pb.
           move spaces to cod-iva.
           move low-value  to ror-chiave.
           move tor-chiave to ror-chiave.
           start rordini key >= ror-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read rordini next at end exit perform end-read
                    if ror-anno       not = tor-anno or
                       ror-num-ordine not = tor-numero
                       exit perform
                    end-if
                    if ror-cod-articolo = rmo-articolo
                       if ( ror-imponib-merce + ror-add-piombo ) =
                          rmo-netto
                          move ror-add-piombo to add-pb
                          move ror-cod-iva    to cod-iva
                          exit perform
                       end-if
                    end-if
                 end-perform
           end-start.

      ***---
       TROVA-ADDIZIONALE-NOTA.
           move 0 to add-pb.
           move spaces to cod-iva
           move low-value  to rno-chiave.
           move tno-chiave to rno-chiave.
           start rnotacr key >= rno-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read rnotacr next at end exit perform end-read
                    if rno-anno   not = tno-anno or
                       rno-numero not = tno-numero
                       exit perform
                    end-if
                    if rno-cod-articolo = rmo-articolo
                       if ( rno-prz-unitario + rno-add-piombo ) =
                          rmo-netto                     
                          move rno-add-piombo to add-pb
                          move rno-cod-iva    to cod-iva
                          exit perform
                       end-if
                    end-if
                 end-perform
           end-start.

      ***---
       TROVA-PROMO.
           set trovato-articolo to false.
           move 0 to add-pb.
           move low-value  to ror-chiave.
           move tor-chiave to ror-chiave.
           start rordini key >= ror-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read rordini next at end exit perform end-read
                    if ror-anno       not = tor-anno or
                       ror-num-ordine not = tor-numero
                       exit perform
                    end-if
                    if ror-cod-articolo  = rmo-articolo and
                       ror-qta           = rmo-qta
                       if ( ror-imponib-merce + ror-add-piombo ) = 
                          rmo-netto
                          set trovato-articolo to true
                          exit perform
                       end-if
                    end-if
                 end-perform
           end-start.
