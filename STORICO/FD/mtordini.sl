       SELECT mtordini
           ASSIGN       TO  "mtordini"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           LOCK MODE    IS MANUAL
           FILE STATUS  IS STATUS-mtordini
           RECORD KEY   IS mto-chiave
           ALTERNATE RECORD KEY IS mto-k-ord-cli = mto-anno, 
           mto-num-ord-cli
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS mto-k-data = mto-data-ordine, 
           mto-chiave
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS mto-k-clides = mto-cod-cli, 
           mto-prg-destino, mto-data-ordine
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS mto-k-age = mto-cod-agente, 
           mto-data-ordine
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-mto-stato-sel = mto-stato-ordine, 
           mto-cod-cli, mto-data-ordine
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-mto-stato = mto-stato-ordine, 
           mto-chiave
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS mto-k-gdo = mto-gdo, 
           mto-data-ordine, mto-chiave
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS mto-k-bloc = mto-stato-attivazione, 
           mto-data-ordine, mto-cod-cli, mto-prg-destino
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-giang = mto-data-note1, mto-chiave
           WITH DUPLICATES .
