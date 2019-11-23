       SELECT contestazioni
           ASSIGN       TO  "contestazioni"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-contestazioni
           RECORD KEY   IS cnt-chiave
           ALTERNATE RECORD KEY IS k1 = cnt-cod-cli, cnt-prg-destino, 
           cnt-tipo, cnt-chiave
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-fattura = cnt-fattura
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-nota = cnt-nota-cr
           WITH DUPLICATES .
