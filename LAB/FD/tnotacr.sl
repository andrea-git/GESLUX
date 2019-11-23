       SELECT tnotacr
           ASSIGN       TO  "tnotacr"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           LOCK MODE    IS MANUAL WITH LOCK ON MULTIPLE RECORDS 
           FILE STATUS  IS STATUS-tnotacr
           RECORD KEY   IS tno-chiave
           ALTERNATE RECORD KEY IS k-causale = tno-causale, tno-anno, 
           tno-numero
           ALTERNATE RECORD KEY IS k1 = tno-cod-cli, tno-prg-destino, 
           tno-anno, tno-numero
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k2 = tno-data-passaggio-ordine, 
           tno-anno, tno-numero
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-fattura = tno-anno-fattura, 
           tno-num-fattura
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k4 = tno-anno-fattura, 
           tno-data-fattura, tno-num-fattura, tno-num-prenot, 
           tno-fatt-prenotata
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-contab = tno-agg-contab, 
           tno-anno-fattura, tno-num-fattura
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-data = tno-data, tno-numero
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-agfatt = tno-anno-fattura, 
           tno-data-fattura, tno-num-fattura, tno-num-prenot, 
           tno-fatt-prenotata, tno-chiave
           ALTERNATE RECORD KEY IS k-andamento-data = tno-agg-contab, 
           tno-data-fattura
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-andamento-cliente = tno-cod-cli, 
           tno-agg-contab, tno-data-fattura
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-andamento-clides = tno-cod-cli, 
           tno-prg-destino, tno-agg-contab, tno-data-fattura
           WITH DUPLICATES .
