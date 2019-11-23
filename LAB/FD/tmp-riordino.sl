       SELECT tmp-riordino
           ASSIGN       TO  path-tmp-riordino
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-tmp-riordino
           RECORD KEY   IS tmp-riord-chiave
           ALTERNATE RECORD KEY IS k-ord = tmp-riord-art-desc, 
           tmp-riord-data, tmp-riord-numero
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-ritiri = tmp-riord-idx, 
           tmp-riord-art-desc, tmp-riord-tipo, tmp-riord-data, 
           tmp-riord-numero
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-articolo = tmp-riord-articolo
           WITH DUPLICATES .
