       SELECT STO-eordini
           ASSIGN       TO  path-sto-eordini
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           LOCK MODE    IS MANUAL WITH ROLLBACK 
           FILE STATUS  IS STATUS-sto-eordini
           RECORD KEY   IS STO-eor-chiave OF STO-eordini.
