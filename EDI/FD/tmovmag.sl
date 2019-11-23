      ** tmo-teva-chiave contiene la chiave dell'evasione o della bozza NC (se merce rotta)
       SELECT tmovmag
           ASSIGN       TO  "tmovmag"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-tmovmag
           RECORD KEY   IS tmo-chiave
           ALTERNATE RECORD KEY IS key01 = tmo-anno, tmo-causale, 
           tmo-numero
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k2 = tmo-tipo, tmo-cod-clifor, 
           tmo-destino, tmo-data-movim
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-data = tmo-data-movim, tmo-numero
           WITH DUPLICATES .
