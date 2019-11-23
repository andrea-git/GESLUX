       SELECT STO-tagli
           ASSIGN       TO  path-sto-tagli
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-sto-tagli
           RECORD KEY   IS STO-tag-chiave
           ALTERNATE RECORD KEY IS k2 = STO-tag-data
           WITH DUPLICATES .
