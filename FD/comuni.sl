       SELECT comuni
           ASSIGN       TO  "comuni"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-Comuni
           RECORD KEY   IS com-chiave
           ALTERNATE RECORD KEY IS com-dati
           WITH DUPLICATES .
