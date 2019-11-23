       SELECT tmp-cumulato
           ASSIGN       TO  path-tmp-cumulato
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-tmp-cumulato
           RECORD KEY   IS tmp-cum-chiave
           ALTERNATE RECORD KEY IS k-ord = tmp-cum-inev, 
           tmp-cum-mar-descrizione
           WITH DUPLICATES .
