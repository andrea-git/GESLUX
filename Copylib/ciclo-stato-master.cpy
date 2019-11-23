      ***---
       CICLO-STATI-ORDINI.
           open i-o mtordini.
           open i-o mrordini.
           open i-o tordini.
           open i-o rordini.

           move low-value to mto-rec.
           set mto-registrato to true.
           start mtordini key >= k-mto-stato
                 invalid  continue
             not invalid
                 perform until 1 = 2

                    read mtordini next at end exit perform end-read
                    move mto-rec to como-rec
                    if mto-chiuso
                       exit perform
                    end-if
                    set ricalcolo to true
                    perform AGGIORNA-STATO-MASTER
                    move como-rec to mto-rec
                    start mtordini key > k-mto-stato
                          invalid  exit perform
                    end-start

                 end-perform
           end-start.

           close mtordini.
           close mrordini.
           close tordini.
           close rordini.

      ***--- DUMMY NON TOCCARE
       AGGIORNA-IMPEGNATO-MASTER.

      ***---
       PARAGRAFO-COPY.
        copy "aggiorna-stato-master.cpy"
