       SELECT tmp-inventory
           ASSIGN       TO  path-tmp
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-tmp-inventory
           RECORD KEY   IS tinv-chiave
           ALTERNATE RECORD KEY IS k-ord = tinv-mag, tinv-marca, 
           tinv-art-descrizione
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-articolo = tinv-mag, 
           tinv-articolo, tinv-imballo, tinv-peso.
