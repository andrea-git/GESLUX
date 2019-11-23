       SELECT STO-brnotacr
           ASSIGN       TO  path-sto-brnotacr
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-sto-brnotacr
           RECORD KEY   IS STO-brno-chiave
           ALTERNATE RECORD KEY IS STO-brno-k-articolo = 
           STO-brno-cod-articolo, STO-brno-chiave
           WITH DUPLICATES .
