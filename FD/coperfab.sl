       SELECT coperfab
           ASSIGN       TO  "coperfab"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-coperfab
           RECORD KEY   IS cpf-chiave
           ALTERNATE RECORD KEY IS k-listino = cpf-listino, cpf-articolo
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-forn = cpf-fornitore, cpf-destino, 
           cpf-chiave
           WITH DUPLICATES .
