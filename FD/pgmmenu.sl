      *Utilizzato dal menu visual per chiamare la versione acu. Per non creare problemi di cancellazione viene usato sempre in append
       SELECT pgmmenu
           ASSIGN       TO  "pgmmenu"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-pgmmenu
           RECORD KEY   IS pmm-chiave.
