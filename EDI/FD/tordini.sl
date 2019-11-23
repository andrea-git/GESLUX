       SELECT tordini
           ASSIGN       TO  "tordini"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           LOCK MODE    IS MANUAL
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
           ALTERNATE RECORD KEY IS k-tipo = tor-tipo, tor-chiave
           ALTERNATE RECORD KEY IS k-data = tor-data-creazione, 
           tor-numero
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-agfatt = tor-anno-fattura, 
           tor-data-fattura, tor-num-fattura, tor-num-prenot, 
           tor-fatt-prenotata, tor-chiave
           ALTERNATE RECORD KEY IS k-stbolle = tor-anno-bolla, 
           tor-data-bolla, tor-num-bolla, tor-bolla-prenotata, 
           tor-chiave
           ALTERNATE RECORD KEY IS k-andamento-data = tor-agg-contab, 
           tor-data-fattura
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-andamento-cliente = tor-cod-cli, 
           tor-agg-contab, tor-data-fattura
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-andamento-clides = tor-cod-cli, 
           tor-prg-destino, tor-agg-contab, tor-data-fattura
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-promo = tor-stato, tor-promo, 
           tor-data-ordine, tor-numero, tor-cod-cli, tor-prg-destino
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-or = tor-cod-cli, tor-prg-destino, 
           tor-num-ord-cli
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-tor-inviare = tor-da-inviare OF 
           tordini, tor-chiave OF tordini
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-tor-tipocli = tor-tipocli OF 
           tordini, tor-cod-cli OF tordini, tor-prg-destino OF tordini, 
           tor-chiave OF tordini
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-tor-gdo = tor-gdo OF tordini, 
           tor-cod-cli OF tordini, tor-prg-destino OF tordini, 
           tor-chiave OF tordini
           WITH DUPLICATES .
