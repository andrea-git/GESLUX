      ***---
       TROVA-PARAMETRO.
           |1. LETTURA COMPLETA
           initialize prm-chiave replacing numeric data by zeroes
                                      alphanumeric data by spaces.
           move como-prm-destino     to prm-destino.
           move como-prm-cliente     to prm-cliente.

           set cli-tipo-C of clienti to true.
           |Dai listini non ho il cliente ma subito il gdo
           if como-prm-cliente = 0        
              move como-prm-gdo     to prm-gdo
              move como-prm-tipocli to prm-tipocli
              perform LETTURA-GDO
           else
              move como-prm-cliente to cli-codice of clienti
              read clienti no lock 
                   invalid initialize cli-rec of clienti 
              end-read
              move cli-gdo  of clienti  to prm-gdo
              move cli-tipo of clienti  to prm-tipocli
              read param no lock
                   invalid
                   perform LETTURA-CLIENTE
               not invalid
                   if prm-gestisci-no
                      perform LETTURA-CLIENTE
                   end-if
              end-read
           end-if.

      ***---
       LETTURA-CLIENTE.
           move 0 to prm-destino.
           read param no lock
                invalid
                perform LETTURA-GDO
            not invalid
                if prm-gestisci-no
                   perform LETTURA-GDO
                end-if
           end-read.

      ***---
       LETTURA-GDO.
           move 0 to prm-cliente.
           read param no lock
                invalid
                perform LETTURA-TIPOCLI
            not invalid
                if prm-gestisci-no
                   perform LETTURA-TIPOCLI
                end-if
           end-read.

      ***---
       LETTURA-TIPOCLI.
           move spaces to prm-gdo.
           read param no lock
                invalid
                perform LETTURA-LUBEX
            not invalid
                if prm-gestisci-no
                   perform LETTURA-LUBEX
                end-if
           end-read.

      ***---
       LETTURA-LUBEX.
           move spaces to prm-tipocli.
           read param no lock
                invalid continue
           end-read.


      ***---
       TROVA-PARAMETRO-GDO.
           |LETTURA-GDO
           initialize prm-chiave replacing numeric data by zeroes
                                      alphanumeric data by spaces.
           move cli-gdo  of clienti to prm-gdo.
           move cli-tipo of clienti to prm-tipocli.

           read param no lock
                invalid
                perform LETTURA-TIPOCLI-GDO
            not invalid
                if prm-gestisci-no or prm-gg-inizio-vol = 0
                   perform LETTURA-TIPOCLI-GDO
                end-if
           end-read.

      ***---
       LETTURA-TIPOCLI-GDO.
           move spaces to prm-gdo.
           read param no lock
                invalid
                perform LETTURA-LUBEX
            not invalid
                if prm-gestisci-no or prm-gg-inizio-vol = 0
                   perform LETTURA-LUBEX
                end-if
           end-read.

      ***---
       LETTURA-LUBEX-GDO.
           move spaces to prm-tipocli.
           read param no lock
                invalid continue
           end-read.
