      *Monorecord
       SELECT EDI-clides
           ASSIGN       TO  "EDI-clides"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-EDI-clides
           RECORD KEY   IS ecd-chiave
           ALTERNATE RECORD KEY IS ecd-k-orders = ecd-cod-dest, 
           ecd-cod-consegna
           WITH DUPLICATES .
