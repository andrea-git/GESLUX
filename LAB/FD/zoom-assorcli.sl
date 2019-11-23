       SELECT zoom-assorcli
           ASSIGN       TO  path-zoom
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-zoom-assorcli
           RECORD KEY   IS zoom-ass-id
           ALTERNATE RECORD KEY IS zoom-ass-cli-ragsoc
           WITH DUPLICATES .
