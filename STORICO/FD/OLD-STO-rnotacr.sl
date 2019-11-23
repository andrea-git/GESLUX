       SELECT OLD-STO-rnotacr
           ASSIGN       TO  path-OLD-STO-rnotacr
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-OLD-STO-rnotacr
           RECORD KEY   IS OLD-STO-rno-chiave
           ALTERNATE RECORD KEY IS OLD-STO-rno-k-articolo = 
           OLD-STO-rno-cod-articolo, OLD-STO-rno-chiave
           WITH DUPLICATES .
