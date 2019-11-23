       SELECT tmp-fatturati
           ASSIGN       TO  path-tmp-fatturati
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-tmp-fatturati
           RECORD KEY   IS tmp-f-chiave
           ALTERNATE RECORD KEY IS k-ord = tmp-f-data, tmp-f-num
           WITH DUPLICATES .
