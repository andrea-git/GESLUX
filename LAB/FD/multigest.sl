      ** Utilizzato per evitare il conflitto tra ricalimp e altri programmi. Monorecord
       SELECT multigest
           ASSIGN       TO  "multigest"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-multigest
           RECORD KEY   IS mul-chiave.
