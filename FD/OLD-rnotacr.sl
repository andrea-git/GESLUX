       SELECT OLD-rnotacr
           ASSIGN       TO  "OLD-rnotacr"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-OLD-rnotacr
           RECORD KEY   IS OLD-rno-chiave
           ALTERNATE RECORD KEY IS OLD-rno-k-articolo = 
           OLD-rno-cod-articolo, 
           OLD-rno-chiave
           WITH DUPLICATES .
