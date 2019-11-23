      ** 2^ GRIGLIA EVASIONI DA ARTICOLO: SERVE PER ORDINARE CLIENTE/DESTINO
      *
       SELECT tmp-mrordini
           ASSIGN       TO  path-tmp-mrordini
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-tmp-mrordini
           RECORD KEY   IS tmro-chiave
           ALTERNATE RECORD KEY IS k-prg = tmro-prg-chiave, tmro-chiave
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-promo = tmro-promo, tmro-chiave
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-blister = tmro-promo, 
           tmro-blister-id
           WITH DUPLICATES .
