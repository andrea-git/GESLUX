      ***---
       RECUPERA-PRZ-LISTINO.
           move 0 to prezzo-listino.
           if age-listino not = 0
              move age-listino        to lis-codice
              move ror-cod-articolo   to lis-articolo
              read lisagente no lock
                   invalid
                   initialize lis-rec replacing numeric data by zeroes
                                           alphanumeric data by spaces
                   move tge-listino-promo to lis-codice
                   read lisagente no lock
                        invalid perform VALORE-DA-ARTICOLI
                    not invalid
                        perform CONTROLLA-PERIODO-VALIDITA
                        if not listino-valido
                           perform VALORE-DA-ARTICOLI
                        else
                           perform AGGIUNGI-PIOMBO
                        end-if
                   end-read
      *****             move prezzo-listino   to r-pz-agente
               not invalid
                   perform CONTROLLA-PERIODO-VALIDITA
                   if not listino-valido
                      perform VALORE-DA-ARTICOLI
                   else
                      perform AGGIUNGI-PIOMBO
                   end-if
      *****             move prezzo-listino   to r-pz-agente
              end-read
           else
              if age-codice not = 0
                 initialize lis-rec replacing numeric data by zeroes
                                         alphanumeric data by spaces
                 move tge-listino-promo to lis-codice
                 move ror-cod-articolo  to lis-articolo
                 read lisagente no lock
                      invalid perform VALORE-DA-ARTICOLI
                  not invalid 
                      perform CONTROLLA-PERIODO-VALIDITA
                      if not listino-valido
                         perform VALORE-DA-ARTICOLI
                      else
                         perform AGGIUNGI-PIOMBO
                      end-if
                 end-read
      *****           move prezzo-listino   to r-pz-agente
              end-if
           end-if.
      ***---
       CONTROLLA-PERIODO-VALIDITA.
           set listino-valido to false.
           if tor-data-ordine >= lis-data-inizio-old and
              tor-data-ordine <= lis-data-fine-old
              move lis-prezzo-old          to prezzo-listino
              set listino-valido to true
           end-if.

           if tor-data-ordine >= lis-data-inizio-new and
              tor-data-ordine <= lis-data-fine-new
              move lis-prezzo-new          to prezzo-listino
              set listino-valido to true
           end-if.

      ***--- 
       VALORE-DA-ARTICOLI.
           if age-perce-marca(1) not = 0
              move 0 to como-valore
           else
              compute como-valore =
                      art-prezzo-vendita - 
                   (( art-prezzo-vendita * 
                      art-perce-sconto-agente ) / 100)

              if ror-si-omaggio
                 compute como-valore =
                         como-valore - 
                      (( como-valore * age-omaggi ) / 100)
              end-if
           end-if.

           move como-valore to prezzo-listino.

      ***---
       AGGIUNGI-PIOMBO.
           if age-si-add-pb
              move tor-data-ordine to como-data-ordine tpb-data 
              |Se devo ricalcolare l'addizionale piombo su un
              |ordine senza data, ossia creato da piu master
              if tor-data-ordine = 0 and tor-da-ordine-si
                 if ror-anno-master    not = 0 and
                    ror-numero-master  not = 0
                    move ror-chiave-ordine-testa to mto-chiave
                    read mtordini no lock
                         invalid continue
                     not invalid 
                         move mto-data-ordine to como-data-ordine 
                                                 tpb-data
                    end-read
                 end-if
              end-if
              move art-marca-prodotto to tpb-marca
              move prezzo-listino     to como-prz-unitario
              perform ADDIZIONALE-PIOMBO-LISTINO
              add add-piombo to como-prz-unitario
                  giving prezzo-listino
           end-if.

      ***---
       ADDIZIONALE-PIOMBO-LISTINO.
           move 0 to add-piombo.
           start tpiombo key <= tpb-chiave
                 invalid continue
             not invalid
                 read tpiombo previous
                 if tpb-marca = art-marca-prodotto and
                    tpb-data <= como-data-ordine

                    accept calcolo-piombo 
                           from environment "CALCOLO_PIOMBO"
                    if nuovo-calcolo-piombo
                       perform TROVA-PARAMETRO
                    else
                       set prm-add-piombo-perce-si to true
                    end-if
                    if prm-add-piombo-perce-si 
                       compute risultato-imposte       =
                               como-prz-unitario
      *****                      - como-imp-cou-cobat
                       if art-auto-cobat
                          compute add-piombo-3dec =
                          ( risultato-imposte * tpb-perce-auto ) / 100
                       else
                          compute add-piombo-3dec =
                          ( risultato-imposte * tpb-perce-moto ) / 100
                       end-if
                       add 0,005 to add-piombo-3dec giving add-piombo
                    else                      
                       compute add-piombo-3dec =
                               art-amperaggio * tpb-euro-ampere
                    end-if   
                 end-if
           end-start. 
