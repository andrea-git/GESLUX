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

              perform OPEN-OUTPUT-TMP-EVAART
              perform OPEN-OUTPUT-TMP-BLIS-EVA

           end-if.

           if mro-evadi-dal <= data-oggi
              perform VALORIZZA-REC
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
           move mto-stato-ordine    to tmro-stato.
           move mto-cod-cli         to tmro-cod-cli.
           move mto-prg-destino     to tmro-prg-destino.
           move mro-blister         to tmro-blister.
           if tmro-si-blister
              if mro-bli-codice  not = save-bli-codice or 
                 mro-qta-imballi not = 0
                 move mro-bli-codice to save-bli-codice
                 add 1 to blister-id
              end-if
              if mro-qta-imballi = 0
                 set tmro-bli-testa-no to true
              else
                 set tmro-bli-testa-si to true
              end-if
              move mro-bli-qta      to tmro-qta-imballi
              move blister-id       to tmro-blister-id
              move mro-bli-codice   to tmro-bli-codice
              move mro-bli-qta      to tmro-bli-qta
           else
              move 0                to tmro-qta-imballi
              move 0                to tmro-blister-id
              move 0                to tmro-bli-codice
              move 0                to tmro-bli-qta
              move mro-qta-imballi  to tmro-qta-imballi
           end-if.
           move mro-des-imballo     to tmro-des-imballo.
           move mto-ritira-in-lubex to tmro-ritira.
           move mto-data-note1      to tmro-data-cons.
           perform DESCRIZIONE-ARTICOLO.
           move mro-promo           to tmro-cod-promo.
           write tmro-rec invalid rewrite tmro-rec continue end-write.

      ***---
       DESCRIZIONE-ARTICOLO.
           move mro-cod-articolo to art-codice.
           read articoli no lock.

           move mro-qta-imballi to imballi-ed.

           initialize tmro-descr.

           call "C$JUSTIFY" using imballi-ed, "L".
           inspect art-descrizione replacing trailing spaces 
                                          by low-value.
           inspect mro-des-imballo replacing trailing spaces 
                                          by low-value.

           if mro-si-blister
              if mro-qta-imballi = 0
                 string  art-descrizione delimited low-value
                         " - "           delimited size
                         mro-des-imballo delimited low-value
                         into tmro-descr
                 end-string
              else
                 string  art-descrizione delimited low-value
                         " - "           delimited size
                         mro-des-imballo delimited low-value
                         " da "          delimited size
                         imballi-ed      delimited spaces
                         into tmro-descr
                 end-string
              end-if
           else
              string  art-descrizione delimited low-value
                      " - "           delimited size
                      mro-des-imballo delimited low-value
                      " da "          delimited size
                      imballi-ed      delimited spaces
                      " x "           delimited size
                      art-udm-imballo delimited size
                      into tmro-descr
              end-string
           end-if.

      ***---
       CALCOLA-DATE.
           move spaces        to tge-chiave.
           read tparamge no lock invalid continue end-read.
           accept data-oggi from century-date.
           compute como-data = function integer-of-date (data-oggi).
           if vecchia-evasione
              add tge-gg-evadi-parziale-OLD to como-data
           end-if.
           compute data-consegna = function date-of-integer (como-data).
           |DATA CONSEGNA: evado parzialmente i master con data di 
           |consegna <= di questa data
           
           compute como-data = function integer-of-date (data-oggi).
           if vecchia-evasione
              add tge-gg-cons-max-OLD to como-data
           end-if.
           compute data-cons-max = function date-of-integer (como-data).
           |DATA DI CONSEGNA MASSIMA: Se il master dev'essere 
           |consegnato oltre questa data non verrà considerato
