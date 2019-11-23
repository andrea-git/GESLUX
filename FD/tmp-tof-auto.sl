      ** TMP testate ordini F automatici: controllo congruenza finale
       SELECT tmp-tof-auto
           ASSIGN       TO  path-tmp-tof-auto
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-tmp-tof-auto
           RECORD KEY   IS tta-chiave
           ALTERNATE RECORD KEY IS tta-k1 = tta-fornitore, tta-destino, 
           tta-dati
           WITH DUPLICATES .
