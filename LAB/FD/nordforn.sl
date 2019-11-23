       SELECT nordforn
           ASSIGN       TO  "nordforn"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           LOCK MODE    IS MANUAL WITH ROLLBACK 
           FILE STATUS  IS STATUS-nordforn
           RECORD KEY   IS nof-chiave OF nordforn.
