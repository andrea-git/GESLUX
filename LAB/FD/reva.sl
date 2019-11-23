       SELECT reva
           ASSIGN       TO  "reva"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-reva
           RECORD KEY   IS reva-chiave
           ALTERNATE RECORD KEY IS reva-chiave-ordf OF reva
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS reva-k-articolo of reva = reva-qta 
           OF reva, reva-chiave OF reva
           WITH DUPLICATES .
