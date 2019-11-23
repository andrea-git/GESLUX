       SELECT destinif
           ASSIGN       TO  "destinif"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-destinif
           RECORD KEY   IS desf-chiave
           ALTERNATE RECORD KEY IS K1 = desf-ragsoc-1, desf-codice, 
           desf-prog
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS desf-k2 = desf-codice, 
           desf-ragsoc-1, desf-prog
           WITH DUPLICATES .
