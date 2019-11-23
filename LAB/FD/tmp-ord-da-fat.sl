       SELECT tmp-ord-da-fat
           ASSIGN       TO  path-tmp-ord-da-fat
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-tmp-ord-da-fat
           RECORD KEY   IS tord-chiave
           ALTERNATE RECORD KEY IS k-ord = tord-intestazione, tord-gdo
           WITH DUPLICATES .
