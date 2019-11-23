      ***---
       CARICA-ARTICOLO.
           move 0 to sw-righe.
           move 0 to qta-tot-c qta-tot-p.
           perform varying mese from 1 by 1 until mese > 12
              move 0 to qta-c(mese)
              move 0 to qta-p(mese)
           end-perform.
           set tutto-ok to true.
           modify gd-titolo, reset-grid = 1.
           perform GD-TITOLO-CONTENT.
           modify gd-mov,  reset-grid = 1.
           perform GD-MOV-CONTENT.
           modify gd-giac, reset-grid = 1.
           perform GD-GIAC-CONTENT.

           move art-marca-prodotto to mar-codice.
           read tmarche no lock
                invalid continue
            not invalid move mar-descrizione to col-marca
           end-read.
           move art-codice             to col-art.
           move art-descrizione        to col-des.
           move art-imballo-standard   to imq-codice.
           read timbalqta no lock invalid continue end-read.
           move imq-qta-imb            to imballi-ed.
           move imq-tipo               to imb-codice.
           read timballi no lock 
                invalid  initialize imb-descrizione
           end-read.
           call "C$JUSTIFY" using imballi-ed, "L".
           inspect imballi-ed replacing trailing x"30" by x"20".
           inspect imb-descrizione replacing trailing spaces
                                                   by low-value.
           initialize col-imb.
           string imb-descrizione delimited low-value
                  " da "          delimited size
                  imballi-ed      delimited spaces
                  " x "           delimited size
                  art-udm-imballo delimited size
                  into col-imb
           end-string.
           modify gd-titolo(2, 1), cell-data = col-marca.
           modify gd-titolo(2, 2), cell-data = col-art.
           modify gd-titolo(2, 3), cell-data = col-des.
           modify gd-titolo(2, 4), cell-data = col-imb.

           initialize prg-chiave replacing numeric data by zeroes
                                      alphanumeric data by spaces.
           move art-codice to prg-cod-articolo.
           read progmag no lock
                invalid set errori to true
            not invalid
                move "Giacenza Attuale"  to col-1
                move prg-giacenza        to col-giac
                modify gd-giac(1, 1), cell-data = col-1
                modify gd-giac(1, 2), cell-data = col-giac
                if prg-giacenza < 0
                   modify gd-giac(1, 2), cell-color = 517
                else
                   modify gd-giac(1, 2), cell-color = 513
                end-if
           end-read.

           if tutto-ok
              move spaces to tge-codice
              read tparamge no lock invalid continue end-read
              subtract 1 from tge-anno giving aaaa-pass
              move     1 to mm-pass
              move     1 to gg-pass
              move tge-anno to aaaa-corr
              move    12 to mm-corr
              move    31 to gg-corr
              move data-start     to data-start-num
              move data-end       to data-end-num
              move art-codice     to rmo-articolo
              move data-start-num to rmo-data-movim
              start rmovmag key is >= k-art-data
                    invalid set errori to true
              end-start
              if tutto-ok
                 perform until 1 = 2
                    read rmovmag next at end exit perform end-read
                    if rmo-articolo not = art-codice
                       exit perform
                    end-if
                    if rmo-data-movim > data-end-num
                       exit perform
                    end-if
                    set trovato to false
                    move rmo-anno  to tmo-anno
                    move rmo-movim to tmo-numero
                    read tmovmag no lock 
                         invalid continue
                     not invalid
                         |Il controllo sullo stato è già sulla riga
                         move tmo-causale to tca-codice
                         read tcaumag no lock
                              invalid continue
                          not invalid
                              if tca-cliente
                                 if tca-movim-giac-pos or
                                    tca-movim-giac-neg
                                    set trovato to true
                                 end-if
                              end-if
                         end-read
                    end-read
                    if trovato
                       move rmo-data-movim(5:2) to mese
                       evaluate true
                       when rmo-data-movim(1:4) = aaaa-pass
                            add rmo-qta  to qta-p(mese) qta-tot-p
                       when rmo-data-movim(1:4) = aaaa-corr
                            add rmo-qta  to qta-c(mese) qta-tot-c
                       end-evaluate
                       if sw-righe = 0
                          move 1               to sw-righe
                          move art-codice      to save-articolo
                          move art-descrizione to save-descr
                       end-if
                    end-if
                 end-perform
              end-if
              if sw-righe = 0 
                 set errori to true
              else
                 move 2 to riga
                 move 1 to mese
                 perform 12 times
                    compute var-qta =   qta-c(mese) - qta-p(mese)
                    move var-qta     to col-var-qta
                    move qta-c(mese) to col-corr
                    move qta-p(mese) to col-pass
                    if var-qta = 0
                       move 0 to var-perce
                    else
                       if qta-p(mese) = 0
                          move 100 to var-perce
                       else
                          compute var-perce = 
                                 (var-qta * 100) / qta-p(mese)
                       end-if
                    end-if
                    evaluate true
                    when var-qta < 0
                         if var-perce > 0
                            compute var-perce = var-perce * -1
                         end-if
                    when var-qta > 0
                         if var-perce < 0
                            compute var-perce = var-perce * -1
                         end-if
                    end-evaluate
                    move var-perce to col-var-perce
                    modify gd-mov(riga, 2), cell-data col-corr
                    modify gd-mov(riga, 3), cell-data col-pass
                    modify gd-mov(riga, 4), cell-data col-var-qta
                    modify gd-mov(riga, 5), cell-data col-var-perce
                    perform COLORE-CELLA
                    add 1 to mese riga
                 end-perform
                 move qta-tot-c to col-corr
                 move qta-tot-p to col-pass
                 modify gd-mov(14, 2), cell-data col-corr
                 modify gd-mov(14, 3), cell-data col-pass
              end-if
           end-if.

           modify gd-mov(14), row-font = Verdana12B-Occidentale.

      ***---
       COLORE-CELLA.
           if qta-c(mese) < 0
              modify gd-mov(riga, 2), cell-color = 517
           else
              modify gd-mov(riga, 2), cell-color = 513
           end-if.

           if qta-p(mese) < 0
              modify gd-mov(riga, 3), cell-color = 517
           else
              modify gd-mov(riga, 3), cell-color = 513
           end-if.

           if var-qta < 0
              modify gd-mov(riga, 4), cell-color = 517
           else
              modify gd-mov(riga, 4), cell-color = 513
           end-if.

           if var-perce < 0
              modify gd-mov(riga, 5), cell-color = 517
           else
              modify gd-mov(riga, 5), cell-color = 513
           end-if.
