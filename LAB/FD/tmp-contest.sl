       SELECT tmp-contest
           ASSIGN       TO  path-tmp-contest
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-tmp-contest
           RECORD KEY   IS tmc-chiave
           ALTERNATE RECORD KEY IS k-ord = tmc-art-descrizione
           WITH DUPLICATES .
