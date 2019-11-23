       SELECT STO-teva
           ASSIGN       TO  path-sto-teva
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-sto-teva
           RECORD KEY   IS STO-teva-chiave OF STO-teva
           ALTERNATE RECORD KEY IS STO-teva-stato of STO-teva = 
           STO-teva-stato OF STO-teva, STO-teva-chiave OF STO-teva
           WITH DUPLICATES .
