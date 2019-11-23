       SELECT tmp-nlistini
           ASSIGN       TO  path-tmp-nlistini
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           LOCK MODE    IS MANUAL WITH ROLLBACK 
           FILE STATUS  IS STATUS-tmp-nlistini
           RECORD KEY   IS tmp-nlis-chiave OF tmp-nlistini.
