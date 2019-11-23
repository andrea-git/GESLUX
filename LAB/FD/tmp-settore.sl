       SELECT tmp-settore
           ASSIGN       TO  path-tmp
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-tmp-settore
           RECORD KEY   IS tms-chiave
           ALTERNATE RECORD KEY IS k-marca = tms-marca, tms-tipo
           WITH DUPLICATES .
