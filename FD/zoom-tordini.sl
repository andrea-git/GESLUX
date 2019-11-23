       SELECT zoom-tordini
           ASSIGN       TO  path-zoom-tordini
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-zoom-tordini
           RECORD KEY   IS zoom-tor-key01 = zoom-tor-chiave
           ALTERNATE RECORD KEY IS zoom-tor-numero
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS zoom-tor-num-fat
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-clidest = zoom-tor-cod-cli, 
           zoom-tor-prg-destino
           WITH DUPLICATES .
