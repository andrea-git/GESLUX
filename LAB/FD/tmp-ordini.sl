       SELECT tmp-ordini
           ASSIGN       TO  path-tmp-ordini
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-tmp-ordini
           RECORD KEY   IS tmp-ord-chiave.
