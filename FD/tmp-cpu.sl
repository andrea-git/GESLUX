       SELECT tmp-cpu
           ASSIGN       TO  path-tmp-cpu
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-tmp-cpu
           RECORD KEY   IS cpu-chiave.
