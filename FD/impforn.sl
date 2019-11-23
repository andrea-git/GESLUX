       SELECT impforn
           ASSIGN       TO  "impforn"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-impforn
           RECORD KEY   IS imf-chiave OF impforn
           ALTERNATE RECORD KEY IS imf-descrizione OF impforn
           WITH DUPLICATES .
