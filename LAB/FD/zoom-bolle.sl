       SELECT zoom-bolle
           ASSIGN       TO  path-zoom-bolle
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-zoom-bolle
           RECORD KEY   IS zoom-bol-chiave
           ALTERNATE RECORD KEY IS zoom-bol-num-fat
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-clidest = zoom-bol-cod-cli, 
           zoom-bol-prg-destino
           WITH DUPLICATES .
