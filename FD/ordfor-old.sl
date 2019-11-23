      *Copia del file ordfor SENZA la distinzione delle qta promo che vengono inglobate in quelle standard. Metodo vecchio tenuto per ragioni di sicurezza.
      *f
       SELECT ordfor-old
           ASSIGN       TO  "ordfor-old"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-ordfor-old
           RECORD KEY   IS ord-chiave-old
           ALTERNATE RECORD KEY IS k-ord = ord-marca-old, 
           ord-art-descrizione-old
           WITH DUPLICATES .
