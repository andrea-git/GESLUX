       SELECT tlistini
           ASSIGN       TO  "tlistini"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-tlistini
           RECORD KEY   IS tlis-chiave OF tlistini
           ALTERNATE RECORD KEY IS tlis-chiave-ricerca OF TLISTINI
           WITH DUPLICATES .
