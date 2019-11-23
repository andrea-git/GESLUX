       SELECT tagli
           ASSIGN       TO  "tagli"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-tagli
           RECORD KEY   IS tag-chiave
           ALTERNATE RECORD KEY IS k2 = tag-data
           WITH DUPLICATES .
