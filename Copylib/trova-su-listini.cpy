      ***---
       TROVA-CODICE-ARTICOLO-ON-LISTINI.
           move spaces to lst-cod-art-cli como-lst-cod-art-cli.
           move low-value to lst-rec.
           move cli-gdo   to lst-gdo.
           move ror-cod-articolo  of rordini to lst-articolo.
           if tor-data-bolla = 0
              accept como-lst-data from century-date
           else
              move tor-data-bolla to como-lst-data
           end-if.
           move high-value to lst-data.
           start listini key <= lst-k-articolo
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read listini previous at end exit perform end-read
                    if lst-gdo      not = cli-gdo or
                       lst-articolo not = ror-cod-articolo of rordini
                       exit perform
                    end-if
                    if lst-data <= como-lst-data
                       move lst-cod-art-cli to como-lst-cod-art-cli
                       exit perform
                    end-if
                 end-perform
           end-start.
