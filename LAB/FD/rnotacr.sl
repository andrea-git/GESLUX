       SELECT rnotacr
           ASSIGN       TO  "rnotacr"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-rnotacr
           RECORD KEY   IS rno-chiave
           ALTERNATE RECORD KEY IS rno-k-articolo = rno-cod-articolo, 
           rno-chiave
           WITH DUPLICATES .
