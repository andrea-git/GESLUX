       SELECT logfile
           ASSIGN       TO  "logfile"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-logfile
           RECORD KEY   IS log-chiave
           ALTERNATE RECORD KEY IS k-chiave = log-chiave-file, log-oper
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-program = log-pgm, log-oper
           WITH DUPLICATES .
