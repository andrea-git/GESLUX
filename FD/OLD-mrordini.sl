       SELECT OLD-mrordini
           ASSIGN       TO  "OLD-mrordini"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           LOCK MODE    IS MANUAL WITH ROLLBACK 
           FILE STATUS  IS STATUS-OLD-mrordini
           RECORD KEY   IS OLD-mro-chiave
           ALTERNATE RECORD KEY IS OLD-mro-k-promo = 
           OLD-mro-promo, OLD-mro-chiave
           ALTERNATE RECORD KEY IS OLD-mro-k-articolo = 
           OLD-mro-cod-articolo, 
           OLD-mro-chiave
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS OLD-mro-k-progr = 
           OLD-mro-chiave-testa, 
           OLD-mro-progr
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS OLD-mro-k-tprev = OLD-mro-promo, 
           OLD-mro-prg-cod-articolo, OLD-mro-chiave
           WITH DUPLICATES .
