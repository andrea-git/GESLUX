       SELECT tmp-tnotacr
           ASSIGN       TO  path-tmp-tnotacr
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
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
           WITH DUPLICATES .
