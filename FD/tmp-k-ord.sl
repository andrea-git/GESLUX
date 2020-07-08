      ** File usato solo per l'ordinamento di master/evasioni/mov magazzino al momento della scelta elenco
       SELECT tmp-k-ord
           ASSIGN       TO  path-tmp-k-ord
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-tmp-k-ord
           RECORD KEY   IS tko-chiave.
