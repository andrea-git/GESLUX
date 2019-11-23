       SELECT OLD-reva
           ASSIGN       TO  "OLD-reva"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-OLD-reva
           RECORD KEY   IS OLD-reva-chiave
           ALTERNATE RECORD KEY IS OLD-reva-chiave-ordf OF OLD-reva
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS OLD-reva-k-articolo of OLD-reva = 
           OLD-reva-qta 
           OF OLD-reva, OLD-reva-chiave OF OLD-reva
           WITH DUPLICATES .
