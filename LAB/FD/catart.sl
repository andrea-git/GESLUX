       SELECT catart
           ASSIGN       TO  "catart"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS status-catart
           RECORD KEY   IS cat-chiave OF catart
           ALTERNATE RECORD KEY IS cat-art-princ of catart = cat-princ 
           OF catart, cat-codice OF catart
           WITH DUPLICATES .
