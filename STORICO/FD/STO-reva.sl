       SELECT STO-reva
           ASSIGN       TO  path-sto-reva
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-sto-reva
           RECORD KEY   IS STO-reva-chiave
           ALTERNATE RECORD KEY IS STO-reva-chiave-ordf OF STO-reva
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS STO-reva-k-articolo of STO-reva = 
           STO-reva-qta OF STO-reva, STO-reva-chiave OF STO-reva
           WITH DUPLICATES .
