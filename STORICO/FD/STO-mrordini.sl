       SELECT STO-mrordini
           ASSIGN       TO  path-sto-mrordini
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           LOCK MODE    IS MANUAL WITH ROLLBACK 
           FILE STATUS  IS STATUS-sto-mrordini
           RECORD KEY   IS STO-mro-chiave
           ALTERNATE RECORD KEY IS STO-mro-k-promo = STO-mro-promo, 
           STO-mro-chiave
           ALTERNATE RECORD KEY IS STO-mro-k-articolo = 
           STO-mro-cod-articolo, STO-mro-chiave
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS STO-mro-k-progr = 
           STO-mro-chiave-testa, STO-mro-progr
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS STO-mro-k-tprev = STO-mro-promo, 
           STO-mro-prg-cod-articolo, STO-mro-chiave
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS STO-mro-k-ord-art = 
           STO-mro-chiave-testa, STO-mro-cod-articolo
           WITH DUPLICATES .
