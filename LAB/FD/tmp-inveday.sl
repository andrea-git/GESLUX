       SELECT tmp-inveday
           ASSIGN       TO  path-tmp
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-tmp-inveday
           RECORD KEY   IS tinv-chiave
           ALTERNATE RECORD KEY IS k-ord = tinv-mag, tinv-marca, 
           tinv-art-descrizione
           WITH DUPLICATES .
