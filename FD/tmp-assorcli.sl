       SELECT tmp-assorcli
           ASSIGN       TO  path-tmp-assorcli
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-tmp-assorcli
           RECORD KEY   IS tmp-asc-chiave
           ALTERNATE RECORD KEY IS tmp-asc-data-inizio-validita
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS tmp-asc-cod-articolo-per-cliente
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS key01 = tmp-asc-cod-gruppo-gdo, 
           tmp-asc-cod-articolo, tmp-asc-cod-cliente, 
           tmp-asc-progressivo-destino
           WITH DUPLICATES .
