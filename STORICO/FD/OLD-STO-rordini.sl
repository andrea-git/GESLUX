       SELECT OLD-STO-rordini
           ASSIGN       TO  PATH-OLD-STO-RORDINI
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           LOCK MODE    IS MANUAL WITH ROLLBACK 
           FILE STATUS  IS STATUS-OLD-STO-rordini
           RECORD KEY   IS OLD-STO-ror-chiave OF OLD-STO-rordini
           ALTERNATE RECORD KEY IS OLD-STO-ror-k-promo = 
           OLD-STO-ror-promo OF OLD-STO-rordini
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS OLD-STO-ror-k-articolo = 
           OLD-STO-ror-cod-articolo OF OLD-STO-rordini, 
           OLD-STO-ror-chiave OF OLD-STO-rordini
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS OLD-STO-ror-k-master = 
           OLD-STO-ror-chiave-ordine OF OLD-STO-rordini, 
           OLD-STO-ror-chiave OF OLD-STO-rordini
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS OLD-STO-ror-k-stbolle = 
           OLD-STO-ror-anno OF OLD-STO-rordini, 
           OLD-STO-ror-num-ordine OF OLD-STO-rordini, 
           OLD-STO-ror-chiave-ordine OF OLD-STO-rordini
           WITH DUPLICATES .
