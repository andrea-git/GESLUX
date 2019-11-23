      *NON TOCCARE!!!! USATO SUL LAB PER I TAGLI!!!! USATO SOLAMENTE IN GORDC
      *G
      *
       SELECT int-rordini
           ASSIGN       TO  "int-rordini"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-int-rordini
           RECORD KEY   IS int-ror-chiave
           ALTERNATE RECORD KEY IS int-ror-k-promo = int-ror-promo
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS int-ror-k-articolo = 
           int-ror-cod-articolo, int-ror-chiave
           WITH DUPLICATES .
