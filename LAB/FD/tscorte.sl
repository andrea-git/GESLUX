       SELECT tscorte
           ASSIGN       TO  "tscorte"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-tscorte
           RECORD KEY   IS sco-chiave
           ALTERNATE RECORD KEY IS sco-descrizione
           WITH DUPLICATES .
