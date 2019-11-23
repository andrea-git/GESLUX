       SELECT STO-tordini
           ASSIGN       TO  path-STO-tordini
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           LOCK MODE    IS MANUAL
           FILE STATUS  IS STATUS-STO-tordini
           RECORD KEY   IS STO-tor-chiave
           ALTERNATE RECORD KEY IS k-causale = STO-tor-causale, 
           STO-tor-anno, STO-tor-numero
           ALTERNATE RECORD KEY IS k1 = STO-tor-cod-cli, 
           STO-tor-prg-destino, STO-tor-anno, STO-tor-numero
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k2 = STO-tor-data-passaggio-ordine, 
           STO-tor-anno, STO-tor-numero
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-bolla = STO-tor-anno-bolla, 
           STO-tor-num-bolla
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k3 = STO-tor-anno-bolla, 
           STO-tor-data-bolla, STO-tor-num-bolla, 
           STO-tor-bolla-prenotata
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-fattura = STO-tor-anno-fattura, 
           STO-tor-num-fattura
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k4 = STO-tor-anno-fattura, 
           STO-tor-data-fattura, STO-tor-num-fattura, 
           STO-tor-num-prenot, STO-tor-fatt-prenotata
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-contab = STO-tor-agg-contab, 
           STO-tor-anno-fattura, STO-tor-num-fattura
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-tipo = STO-tor-tipo, STO-tor-chiave
           ALTERNATE RECORD KEY IS k-data = STO-tor-data-creazione, 
           STO-tor-numero
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-agfatt = STO-tor-anno-fattura, 
           STO-tor-data-fattura, STO-tor-num-fattura, 
           STO-tor-num-prenot, STO-tor-fatt-prenotata, STO-tor-chiave
           ALTERNATE RECORD KEY IS k-stbolle = STO-tor-anno-bolla, 
           STO-tor-data-bolla, STO-tor-num-bolla, 
           STO-tor-bolla-prenotata, STO-tor-chiave
           ALTERNATE RECORD KEY IS k-andamento-data = 
           STO-tor-agg-contab, STO-tor-data-fattura
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-andamento-cliente = 
           STO-tor-cod-cli, STO-tor-agg-contab, STO-tor-data-fattura
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-andamento-clides = 
           STO-tor-cod-cli, STO-tor-prg-destino, STO-tor-agg-contab, 
           STO-tor-data-fattura
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-promo = STO-tor-stato, 
           STO-tor-promo, STO-tor-data-ordine, STO-tor-numero, 
           STO-tor-cod-cli, STO-tor-prg-destino
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-or = STO-tor-cod-cli, 
           STO-tor-prg-destino, STO-tor-num-ord-cli
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-tor-inviare = STO-tor-da-inviare 
           OF STO-tordini, STO-tor-chiave OF STO-tordini
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-tor-tipocli = STO-tor-tipocli OF 
           STO-tordini, STO-tor-cod-cli OF STO-tordini, 
           STO-tor-prg-destino OF STO-tordini, STO-tor-chiave OF 
           STO-tordini
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-tor-gdo = STO-tor-gdo OF 
           STO-tordini, STO-tor-cod-cli OF STO-tordini, 
           STO-tor-prg-destino OF STO-tordini, STO-tor-chiave OF 
           STO-tordini
           WITH DUPLICATES .
