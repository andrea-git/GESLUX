       SELECT tmp-perage
           ASSIGN       TO  path-tmp-perage
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-tmp-perage
           RECORD KEY   IS tmp-per-chiave
           ALTERNATE RECORD KEY IS k-ord = tmp-per-chiave, 
           tmp-per-cli-ragsoc, tmp-per-art-descrizione
           WITH DUPLICATES .
