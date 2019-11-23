       SELECT OLD-rordini
           ASSIGN       TO  "OLD-rordini"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           LOCK MODE    IS MANUAL WITH ROLLBACK 
           FILE STATUS  IS STATUS-OLD-rordini
           RECORD KEY   IS OLD-ror-chiave OF OLD-rordini
           ALTERNATE RECORD KEY IS OLD-ror-k-promo = 
           OLD-ror-promo OF OLD-rordini
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS OLD-ror-k-articolo = 
           OLD-ror-cod-articolo OF 
           OLD-rordini, OLD-ror-chiave OF OLD-rordini
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS OLD-ror-k-master = 
           OLD-ror-chiave-ordine OF 
           OLD-rordini, OLD-ror-chiave OF OLD-rordini
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS OLD-ror-k-stbolle = 
           OLD-ror-anno OF OLD-rordini, 
           OLD-ror-num-ordine OF OLD-rordini, 
           OLD-ror-chiave-ordine OF OLD-rordini
           WITH DUPLICATES .
