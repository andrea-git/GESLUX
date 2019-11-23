       SELECT tmp-stat
           ASSIGN       TO  path-tmp
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-tmp-stat
           RECORD KEY   IS tst-chiave
           ALTERNATE RECORD KEY IS tst-kmarca-alfa = tst-des-marca
           WITH DUPLICATES .
