      ** File contenenti le chiavi dei master (teste) filtrati dal primo giro dell'evasione clienti
       SELECT tmp-k-mtordini
           ASSIGN       TO  path-tmp-k-mtordini
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           LOCK MODE    IS MANUAL
           FILE STATUS  IS STATUS-tmp-k-mtordini
           RECORD KEY   IS k-mto-chiave
           ALTERNATE RECORD KEY IS tmp-k-promo = k-mto-promo, 
           k-mto-chiave
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS tmp-k-valutare = k-mto-evadibile, 
           k-mto-chiave
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS tmp-k-valutare-blister = 
           k-mto-evadibile, k-mto-blister, k-mto-chiave
           WITH DUPLICATES .
