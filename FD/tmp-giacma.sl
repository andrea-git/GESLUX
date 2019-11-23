       SELECT tmp-giacma
           ASSIGN       TO  path-tmp
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-tmp-giacma
           RECORD KEY   IS tmg-chiave
           ALTERNATE RECORD KEY IS k-marca = tmg-des-marca, tmg-chiave
           WITH DUPLICATES .
