      ***---
       TROVA-CODICE-ARTICOLO-ON-ASSORCLI.
           set trovato-assorcli to false.
           |1. Cerco per chiave completa
           move spaces           to asc-cod-articolo-per-cliente.
           move cli-gdo          to asc-cod-gruppo-gdo.
           move cli-codice       to asc-cod-cliente.
           move tor-prg-destino  to asc-progressivo-destino.
           move ror-cod-articolo of rordini to asc-cod-articolo.
           read assorcli no lock 
                invalid continue 
            not invalid set trovato-assorcli to true
           end-read.

           if not trovato-assorcli
              |2. Elimino il destino
              move 0 to asc-progressivo-destino
              read assorcli no lock
                   invalid  continue
               not invalid  set trovato-assorcli to true
              end-read
           end-if.

           if not trovato-assorcli
              |3. Elimino il cliente cercando così
              |   solamente per gruppo-codice
              move 0 to asc-cod-cliente
              read assorcli no  lock
                   invalid  continue
               not invalid  set trovato-assorcli to true
              end-read
           end-if.

           if not trovato-assorcli
              |4. Cerco solo per cliente-articolo solo
              |   se non gdo altrimenti è = al punto 1
              move cli-tipo to tcl-codice
              read ttipocli no lock invalid continue end-read
              if tcl-gdo-si or tcl-gdo-opz
      *****        if cli-gdo not = spaces
                 move spaces     to asc-cod-gruppo-gdo
                 move cli-codice to asc-cod-cliente
                 read assorcli no lock 
                      invalid continue 
                 end-read
              end-if
           end-if.

           if not trovato-assorcli
              move spaces to asc-cod-articolo-per-cliente
           end-if.
