       SELECT OLD-STO-reva
           ASSIGN       TO  path-OLD-STO-reva
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-OLD-STO-reva
           RECORD KEY   IS OLD-STO-reva-chiave
           ALTERNATE RECORD KEY IS 
           OLD-STO-reva-chiave-ordf OF OLD-STO-reva
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS 
           OLD-STO-reva-k-articolo of OLD-STO-reva = 
           OLD-STO-reva-qta OF OLD-STO-reva, 
           OLD-STO-reva-chiave OF OLD-STO-reva
           WITH DUPLICATES .
