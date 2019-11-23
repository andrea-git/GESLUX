      ** STO-tmo-teva-chiave contiene la chiave dell'evasione o della bozza NC (se merce rotta)
       SELECT STO-tmovmag
           ASSIGN       TO  path-sto-tmovmag
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-STO-tmovmag
           RECORD KEY   IS STO-tmo-chiave
           ALTERNATE RECORD KEY IS key01 = STO-tmo-anno, 
           STO-tmo-causale, STO-tmo-numero
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k2 = STO-tmo-tipo, 
           STO-tmo-cod-clifor, STO-tmo-destino, STO-tmo-data-movim
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-data = STO-tmo-data-movim, 
           STO-tmo-numero
           WITH DUPLICATES .
