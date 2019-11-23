       SELECT eordini
           ASSIGN       TO  "eordini"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           LOCK MODE    IS MANUAL WITH ROLLBACK 
           FILE STATUS  IS STATUS-eordini
           RECORD KEY   IS eor-chiave OF eordini.
