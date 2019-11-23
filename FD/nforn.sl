       SELECT nforn
           ASSIGN       TO  "nforn"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           LOCK MODE    IS MANUAL
           FILE STATUS  IS STATUS-nforn
           RECORD KEY   IS nfor-chiave OF nforn.
