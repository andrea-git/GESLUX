       SELECT tmarche
           ASSIGN       TO  "tmarche"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-tmarche
           RECORD KEY   IS mar-chiave
           ALTERNATE RECORD KEY IS mar-descrizione
           WITH DUPLICATES .
