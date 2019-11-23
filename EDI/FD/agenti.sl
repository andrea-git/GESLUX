       SELECT agenti
           ASSIGN       TO  "agenti"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-agenti
           RECORD KEY   IS age-chiave
           ALTERNATE RECORD KEY IS age-ragsoc-1
           WITH DUPLICATES .
