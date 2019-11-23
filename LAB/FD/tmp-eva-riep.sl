      *riepilogo evasioni (griglia)
       SELECT tmp-eva-riep
           ASSIGN       TO  path-tmp-eva-riep
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-tmp-eva-riep
           RECORD KEY   IS ter-chiave.
