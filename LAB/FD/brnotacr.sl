       SELECT brnotacr
           ASSIGN       TO  "brnotacr"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-brnotacr
           RECORD KEY   IS brno-chiave
           ALTERNATE RECORD KEY IS brno-k-articolo = brno-cod-articolo, 
           brno-chiave
           WITH DUPLICATES .
