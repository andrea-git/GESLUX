       SELECT tmp-marca
           ASSIGN       TO  path-tmp
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-tmp-marca
           RECORD KEY   IS tma-chiave
           ALTERNATE RECORD KEY IS tmp-marca-alfa = tma-des, tma-chiave
           WITH DUPLICATES .
