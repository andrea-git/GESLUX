       SELECT nlistini
           ASSIGN       TO  "nlistini"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           LOCK MODE    IS MANUAL WITH ROLLBACK 
           FILE STATUS  IS STATUS-nlistini
           RECORD KEY   IS nlis-chiave OF nlistini.
