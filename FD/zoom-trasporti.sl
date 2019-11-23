       SELECT zoom-trasporti
           ASSIGN       TO  path-zoom-trasporti
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS status-zoom-trasporti
           RECORD KEY   IS zoom-trs-chiave
           ALTERNATE RECORD KEY IS zoom-trs-num-bolla
           WITH DUPLICATES .
