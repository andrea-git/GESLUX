       SELECT OLD-STO-mrordini
           ASSIGN       TO  path-OLD-STO-mrordini
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           LOCK MODE    IS MANUAL WITH ROLLBACK 
           FILE STATUS  IS STATUS-OLD-STO-mrordini
           RECORD KEY   IS OLD-STO-mro-chiave
           ALTERNATE RECORD KEY IS OLD-STO-mro-k-promo = 
           OLD-STO-mro-promo, 
           OLD-STO-mro-chiave
           ALTERNATE RECORD KEY IS OLD-STO-mro-k-articolo = 
           OLD-STO-mro-cod-articolo, OLD-STO-mro-chiave
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS OLD-STO-mro-k-progr = 
           OLD-STO-mro-chiave-testa, OLD-STO-mro-progr
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS OLD-STO-mro-k-tprev = 
           OLD-STO-mro-promo, 
           OLD-STO-mro-prg-cod-articolo, OLD-STO-mro-chiave
           WITH DUPLICATES .
