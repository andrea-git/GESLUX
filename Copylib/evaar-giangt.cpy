      ***---
       SCRIVI-TMP-GD2.
           if prima-volta
              move 0 to tot-idx-giac
              set prima-volta to false
              if path-tmp-mrordini not = spaces
                 close       tmp-mrordini
                 delete file tmp-mrordini
              end-if
              accept como-data from century-date
              accept como-ora  from time
              initialize path-tmp-mrordini
              accept  path-tmp-mrordini from environment "PATH_ST"
              inspect path-tmp-mrordini replacing trailing 
                                        spaces by low-value
              string  path-tmp-mrordini delimited low-value
                      "TMP-MRORDINI_"   delimited size
                      como-data         delimited size
                      "_"               delimited size
                      como-ora          delimited size
                      ".tmp"            delimited size
                      into path-tmp-mrordini
              end-string
              open output tmp-mrordini
              close       tmp-mrordini
              open i-o    tmp-mrordini
              
              if path-tmp-evaart not = spaces
                 close       tmp-evaart
                 delete file tmp-evaart
              end-if
              accept como-data from century-date
              accept como-ora  from time
              initialize path-tmp-evaart
              accept  path-tmp-evaart from environment "PATH_ST"
              inspect path-tmp-evaart replacing trailing 
                                      spaces by low-value
              string  path-tmp-evaart delimited low-value
                      "TMP-EVAART_"   delimited size
                      como-data       delimited size
                      "_"             delimited size
                      como-ora        delimited size
                      ".tmp"          delimited size
                      into path-tmp-evaart
              end-string  
              open output tmp-evaart
              close       tmp-evaart
              open i-o    tmp-evaart

           end-if.
                                
           if mro-evadi-dal <= data-oggi
              if mro-si-blister
                 perform EVADIBILITA-BLISTER
              else
                 perform VALORIZZA-REC
                 if primo-lancio
                    perform TROVA-GIACENZA
                 end-if
              end-if
           end-if.

      ***---
       VALORIZZA-REC.
           if not primo-lancio exit paragraph end-if.
           if mro-promo = 0
              set tmro-promo-no to true
              move 0 to tmro-ini-volantino
           else
              set  tmro-promo-si to true
              move mro-promo to tpr-codice
              read tpromo no lock invalid continue end-read
              move tpr-ini-volantino to tmro-ini-volantino
           end-if.
           move mro-anno            to tmro-anno.
           move mro-numero          to tmro-numero.
           move mro-riga            to tmro-riga.
           move col1-cliente        to tmro-cliente.
           move col1-destino        to tmro-destino.
           move mro-prg-chiave      to tmro-prg-chiave.
           move mro-qta             to tmro-qta-o.
           move mro-qta-e           to tmro-qta-e tmro-qta-evadibile.
           move 0                   to tmro-qta.
           move mro-qta-imballi     to tmro-qta-imballi.
           move mto-stato-ordine    to tmro-stato.
           move mto-cod-cli         to tmro-cod-cli.
           move mto-prg-destino     to tmro-prg-destino.
           move mro-blister         to tmro-blister.
           move mro-des-imballo     to tmro-des-imballo.
           move mto-ritira-in-lubex to tmro-ritira.
           move mto-data-note1      to tmro-data-cons.
           perform DESCRIZIONE-ARTICOLO.
           write tmro-rec invalid rewrite tmro-rec continue end-write.
