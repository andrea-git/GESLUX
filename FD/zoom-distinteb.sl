       SELECT zoom-distinteb
           ASSIGN       TO  path-zoom-distinteb
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-zoom-distinteb
           RECORD KEY   IS zoom-dis-chiave
           ALTERNATE RECORD KEY IS k-des = zoom-dis-art-descrizione, 
           zoom-dis-chiave
           WITH DUPLICATES .
