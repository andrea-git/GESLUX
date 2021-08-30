       SELECT tmp-ricerca-clienti
           ASSIGN       TO  path-tmp-ricerca-clienti
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-tmp-ricerca-clienti
           RECORD KEY   IS trc-cli-chiave
      *SEVE SOLO PER LO ZOOM
      *]
      *-
      *E
           ALTERNATE RECORD KEY IS trc-cli-ragsoc-1
           WITH DUPLICATES .
