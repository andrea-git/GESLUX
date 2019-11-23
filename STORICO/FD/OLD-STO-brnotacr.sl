       SELECT OLD-STO-brnotacr
           ASSIGN       TO  path-OLD-STO-brnotacr
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-OLD-STO-brnotacr
           RECORD KEY   IS OLD-STO-brno-chiave
           ALTERNATE RECORD KEY IS OLD-STO-brno-k-articolo = 
           OLD-STO-brno-cod-articolo, OLD-STO-brno-chiave
           WITH DUPLICATES .
