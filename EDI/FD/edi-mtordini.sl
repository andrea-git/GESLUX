       SELECT EDI-mtordini
           ASSIGN       TO  "EDI-mtordini"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           LOCK MODE    IS MANUAL
           FILE STATUS  IS STATUS-EDI-mtordini
           RECORD KEY   IS emto-chiave
           ALTERNATE RECORD KEY IS emto-k-ord-cli = emto-anno, 
           emto-num-ord-cli
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-emto-stato = emto-stato, 
           emto-chiave
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-emto-data = emto-data-ordine, 
           emto-chiave
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-emto-clides = emto-cod-cli, 
           emto-prg-destino, emto-chiave
           WITH DUPLICATES .
