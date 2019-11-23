       SELECT OLD-brnotacr
           ASSIGN       TO  "OLD-brnotacr"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-OLD-brnotacr
           RECORD KEY   IS OLD-brno-chiave
           ALTERNATE RECORD KEY IS OLD-brno-k-articolo = 
           OLD-brno-cod-articolo, 
           OLD-brno-chiave
           WITH DUPLICATES .
