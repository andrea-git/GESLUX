       SELECT STO-rnotacr
           ASSIGN       TO  path-sto-rnotacr
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-sto-rnotacr
           RECORD KEY   IS STO-rno-chiave
           ALTERNATE RECORD KEY IS STO-rno-k-articolo = 
           STO-rno-cod-articolo, STO-rno-chiave
           WITH DUPLICATES .
