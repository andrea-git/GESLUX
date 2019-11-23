       SELECT rordini
           ASSIGN       TO  "rordini"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           LOCK MODE    IS MANUAL WITH ROLLBACK 
           FILE STATUS  IS STATUS-rordini
           RECORD KEY   IS ror-chiave OF rordini
           ALTERNATE RECORD KEY IS ror-k-promo = ror-promo OF rordini
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS ror-k-articolo = ror-cod-articolo OF 
           rordini, ror-chiave OF rordini
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS ror-k-master = ror-chiave-ordine OF 
           rordini, ror-chiave OF rordini
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS ror-k-stbolle = ror-anno OF rordini, 
           ror-num-ordine OF rordini, ror-chiave-ordine OF rordini
           WITH DUPLICATES .
