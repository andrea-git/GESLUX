       SELECT tmp-sellout
           ASSIGN       TO  path-tmp-sellout
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-tmp-sellout
           RECORD KEY   IS tms-chiave
           ALTERNATE RECORD KEY IS k-ord = tms-descr
           WITH DUPLICATES .
