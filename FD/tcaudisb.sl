       SELECT tcaudisb
           ASSIGN       TO  "tcaudisb"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-tcaudisb
           RECORD KEY   IS tdc-chiave
           ALTERNATE RECORD KEY IS tdc-descrizione
           WITH DUPLICATES .
