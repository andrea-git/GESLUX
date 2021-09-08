       SELECT STO-rordini
           ASSIGN       TO  PATH-STO-RORDINI
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           LOCK MODE    IS MANUAL WITH ROLLBACK 
           FILE STATUS  IS STATUS-STO-rordini
           RECORD KEY   IS STO-ror-chiave OF STO-rordini
           ALTERNATE RECORD KEY IS STO-ror-k-promo = STO-ror-promo OF 
           STO-rordini
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS STO-ror-k-articolo = 
           STO-ror-cod-articolo OF STO-rordini, STO-ror-chiave OF 
           STO-rordini
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS STO-ror-k-master = 
           STO-ror-chiave-ordine OF STO-rordini, STO-ror-chiave OF 
           STO-rordini
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS STO-ror-k-stbolle = STO-ror-anno OF 
           STO-rordini, STO-ror-num-ordine OF STO-rordini, 
           STO-ror-chiave-ordine OF STO-rordini
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS STO-ror-k-ord-art = STO-ror-anno OF 
           STO-rordini, STO-ror-num-ordine OF STO-rordini, 
           STO-ror-cod-articolo
           WITH DUPLICATES .
