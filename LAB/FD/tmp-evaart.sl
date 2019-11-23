      ** ANTEPRIMA 3^ GRIGLIA RAGGRUPPA LE SCELTE PER CLIENTE/DESTINO/ARTICOLO
      *
       SELECT tmp-evaart
           ASSIGN       TO  path-tmp-evaart
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-tmp-evaart
           RECORD KEY   IS tmeva-chiave
           ALTERNATE RECORD KEY IS tmeva-k-ord = tmeva-cod-cli, 
           tmeva-prg-destino, tmeva-ritira, tmeva-chiave
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS tmeva-k-ord-GDO = tmeva-chiave, 
           tmeva-cod-cli, tmeva-prg-destino, tmeva-ritira
           WITH DUPLICATES .
