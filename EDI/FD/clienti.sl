       SELECT clienti
           ASSIGN       TO  "clienti"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-clienti
           RECORD KEY   IS cli-chiave OF clienti
           ALTERNATE RECORD KEY IS cli-K1 of clienti = cli-tipo-CF OF 
           clienti, cli-ragsoc-1 OF clienti, cli-codice OF clienti
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS cli-K3 of clienti = cli-gdo OF 
           clienti, cli-chiave OF clienti
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS cli-K4 of clienti = cli-utf OF 
           clienti, cli-chiave OF clienti
           WITH DUPLICATES 
      *SEVE SOLO PER LO ZOOM
      *]
      *-
      *E
           ALTERNATE RECORD KEY IS cli-ragsoc-1 OF clienti
           WITH DUPLICATES .
