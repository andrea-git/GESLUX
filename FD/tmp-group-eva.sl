      ** File di collegamento per le evasioni master (raggruppamento cliente/destino)
       SELECT tmp-group-eva
           ASSIGN       TO  path-tmp-group-eva
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-tmp-group-eva
           RECORD KEY   IS tgr-chiave
           ALTERNATE RECORD KEY IS tgr-k-ord = tgr-cli-ragsoc, 
           tgr-des-ragsoc, tgr-destino, tgr-evasione
           WITH DUPLICATES .
