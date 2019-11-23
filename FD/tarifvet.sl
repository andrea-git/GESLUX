       SELECT tarifvet
           ASSIGN       TO  "tarifvet"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           LOCK MODE    IS MANUAL WITH LOCK ON MULTIPLE RECORDS 
           FILE STATUS  IS STATUS-tarifvet
           RECORD KEY   IS tfv-chiave
           ALTERNATE RECORD KEY IS k2 = tfv-codice, tfv-campo1, 
           tfv-campo2
           WITH DUPLICATES .
