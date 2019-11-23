       SELECT mrordini
           ASSIGN       TO  "mrordini"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           LOCK MODE    IS MANUAL WITH ROLLBACK 
           FILE STATUS  IS STATUS-mrordini
           RECORD KEY   IS mro-chiave
           ALTERNATE RECORD KEY IS mro-k-promo = mro-promo, mro-chiave
           ALTERNATE RECORD KEY IS mro-k-articolo = mro-cod-articolo, 
           mro-chiave
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS mro-k-progr = mro-chiave-testa, 
           mro-progr
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS mro-k-tprev = mro-promo, 
           mro-prg-cod-articolo, mro-chiave
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS mro-k-ord-art = mro-chiave-testa, 
           mro-cod-articolo
           WITH DUPLICATES .
