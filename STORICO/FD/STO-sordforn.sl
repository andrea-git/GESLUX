       SELECT STO-sordforn
           ASSIGN       TO  path-sto-sordforn
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           LOCK MODE    IS MANUAL WITH ROLLBACK 
           FILE STATUS  IS STATUS-sto-sordforn
           RECORD KEY   IS STO-sof-chiave OF STO-sordforn.
