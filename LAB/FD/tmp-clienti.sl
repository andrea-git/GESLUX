       SELECT tmp-clienti
           ASSIGN       TO  path-tmp-clienti
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-tmp-clienti
           RECORD KEY   IS stcli-chiave
      *SEVE SOLO PER LO ZOOM
      *]
      *-
      *0
      *]
           ALTERNATE RECORD KEY IS stcli-ragsoc-1
           WITH DUPLICATES .
