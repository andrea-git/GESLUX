      * In comune a evasioni da articolo normale e GDO
      ***---
       CHIUDI-MASTER.
           |Nel caso in cui il flag "TENERE SALDI" NON sia 
           |valorizzato, chiudere l'ordine anche in caso 
           |di evasione parziale 
           set cli-tipo-C to true.
           move mto-cod-cli to cli-codice.
           read clienti no lock invalid continue end-read.
           if mto-prg-destino not = 0
              move mto-cod-cli     to des-codice
              move mto-prg-destino to des-prog
              read destini no lock invalid continue end-read
              move des-saldi-banco to cli-saldi-banco
           end-if.
           if cli-saldi-banco-no
              set tutto-ok  to true
              set RecLocked to false
              initialize geslock-linkage
              move "mtordini" to geslock-nome-file
              set tutto-ok to true
              read mtordini lock key mto-chiave 
                   invalid continue 
              end-read
              perform until 1 = 2
                 if not RecLocked
                    exit perform
                 end-if
                 move mto-numero to numero-ed
                 set RecLocked to false
                 initialize geslock-messaggio
                 string "Impossibile chiudere " delimited size
                        "l'ordine n. "          delimited size
                        numero-ed               delimited size
                        into geslock-messaggio
                 end-string
                 move 1 to geslock-v-riprova
                 move 1 to geslock-v-ignora
                 move 0 to geslock-v-termina
                 call   "geslock" using geslock-linkage
                 cancel "geslock"
                 evaluate true
                 when riprova 
                      read mtordini lock key mto-chiave
                           invalid continue 
                      end-read
                 when ignora exit perform
                 end-evaluate
              end-perform
      
              if not RecLocked
                 set mto-chiuso to true
                 rewrite mto-rec invalid continue end-rewrite
              end-if
              unlock mtordini all records
           end-if.
