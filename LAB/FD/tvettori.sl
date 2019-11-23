       SELECT tvettori
           ASSIGN       TO  "tvettori"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-tvettori
           RECORD KEY   IS vet-chiave
           ALTERNATE RECORD KEY IS k-des = vet-descrizione
           WITH DUPLICATES .
