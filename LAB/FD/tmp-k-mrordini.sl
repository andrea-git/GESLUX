      ** Usato dall'evasione clienti per tenere le righe valide
       SELECT tmp-k-mrordini
           ASSIGN       TO  path-tmp-k-mrordini
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-tmp-k-mrordini
           RECORD KEY   IS k-mro-chiave
           ALTERNATE RECORD KEY IS k-mro-promo = k-mro-promo, 
           k-mro-articolo, k-mro-chiave
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-mro-blister = k-mro-chiave, 
           k-mro-bli-codice
           WITH DUPLICATES .
