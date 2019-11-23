       SELECT sordforn
           ASSIGN       TO  "sordforn"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           LOCK MODE    IS MANUAL WITH ROLLBACK 
           FILE STATUS  IS STATUS-sordforn
           RECORD KEY   IS sof-chiave OF sordforn.
