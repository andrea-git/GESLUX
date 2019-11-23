       SELECT tmp-assorcli-zoom
           ASSIGN       TO  path-tmp-assorcli-zoom
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-tmp-assorcli-zoom
           RECORD KEY   IS tmp-asc-key01 = tmp-asc-z-chiave
           ALTERNATE RECORD KEY IS tmp-asc-z-cli-ragsoc-1
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS key01 = tmp-asc-z-art-descrizione
           WITH DUPLICATES .
