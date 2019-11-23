       SELECT tmp-tordini
           ASSIGN       TO  path-tmp-tordini
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-tordini
           RECORD KEY   IS tor-chiave
           ALTERNATE RECORD KEY IS k-causale = tor-causale, tor-anno, 
           tor-numero
           ALTERNATE RECORD KEY IS k1 = tor-cod-cli, tor-prg-destino, 
           tor-anno, tor-numero
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k2 = tor-data-passaggio-ordine, 
           tor-anno, tor-numero
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-bolla = tor-anno-bolla, 
           tor-num-bolla
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k3 = tor-anno-bolla, tor-data-bolla, 
           tor-num-bolla, tor-bolla-prenotata
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-fattura = tor-anno-fattura, 
           tor-num-fattura
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k4 = tor-anno-fattura, 
           tor-data-fattura, tor-num-fattura, tor-num-prenot, 
           tor-fatt-prenotata
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-contab = tor-agg-contab, 
           tor-anno-fattura, tor-num-fattura
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-tipo = tor-tipo, tor-chiave.
