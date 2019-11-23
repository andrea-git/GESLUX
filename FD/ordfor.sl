       SELECT ordfor
           ASSIGN       TO  "ordfor"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-ordfor
           RECORD KEY   IS ord-chiave
           ALTERNATE RECORD KEY IS k-ord = ord-marca, 
           ord-art-descrizione
           WITH DUPLICATES .
