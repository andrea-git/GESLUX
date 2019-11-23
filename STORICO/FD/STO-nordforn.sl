       SELECT STO-nordforn
           ASSIGN       TO  path-sto-nordforn
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           LOCK MODE    IS MANUAL WITH ROLLBACK 
           FILE STATUS  IS STATUS-sto-nordforn
           RECORD KEY   IS STO-nof-chiave OF STO-nordforn.
