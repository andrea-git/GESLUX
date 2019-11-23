       SELECT tmp-movmag
           ASSIGN       TO  path-tmp-movmag
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-tmp-movmag
           RECORD KEY   IS tmp-mov-chiave
           ALTERNATE RECORD KEY IS k-cod-cli = tmp-mov-tipo, 
           tmp-mov-codice
           WITH DUPLICATES .
