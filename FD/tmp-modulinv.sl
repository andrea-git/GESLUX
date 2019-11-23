       SELECT tmp-modulinv
           ASSIGN       TO  path-tmp
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-tmp-modulinv
           RECORD KEY   IS tmod-chiave
           ALTERNATE RECORD KEY IS k-ord = tmod-mag, tmod-marca, 
           tmod-art-descrizione
           WITH DUPLICATES .
