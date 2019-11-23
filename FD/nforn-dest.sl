       SELECT nforn-dest
           ASSIGN       TO  "nforn-dest"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           LOCK MODE    IS MANUAL
           FILE STATUS  IS STATUS-nforn-dest
           RECORD KEY   IS nfod-chiave OF nforn-dest.
