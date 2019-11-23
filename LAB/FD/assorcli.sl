       SELECT assorcli
           ASSIGN       TO  "assorcli"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-assorcli
           RECORD KEY   IS asc-chiave OF assorcli
           ALTERNATE RECORD KEY IS asc-data-inizio-validita OF assorcli
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS asc-cod-articolo-per-cliente OF 
           assorcli
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k1 = asc-cod-gruppo-gdo OF assorcli, 
           asc-cod-articolo OF assorcli, asc-cod-cliente OF assorcli, 
           asc-progressivo-destino OF assorcli
           WITH DUPLICATES .
