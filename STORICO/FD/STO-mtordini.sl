       SELECT STO-mtordini
           ASSIGN       TO  path-STO-mtordini
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           LOCK MODE    IS MANUAL
           FILE STATUS  IS STATUS-STO-mtordini
           RECORD KEY   IS STO-mto-chiave
           ALTERNATE RECORD KEY IS mto-k-ord-cli = STO-mto-anno, 
           STO-mto-num-ord-cli
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS mto-k-data = STO-mto-data-ordine, 
           STO-mto-chiave
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS mto-k-clides = STO-mto-cod-cli, 
           STO-mto-prg-destino, STO-mto-data-ordine
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS mto-k-age = STO-mto-cod-agente, 
           STO-mto-data-ordine
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-mto-stato-sel = 
           STO-mto-stato-ordine, STO-mto-cod-cli, STO-mto-data-ordine
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-mto-stato = STO-mto-stato-ordine, 
           STO-mto-chiave
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS mto-k-gdo = STO-mto-gdo, 
           STO-mto-data-ordine, STO-mto-chiave
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS mto-k-bloc = 
           STO-mto-stato-attivazione, STO-mto-data-ordine, 
           STO-mto-cod-cli, STO-mto-prg-destino
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-giang = STO-mto-data-note1, 
           STO-mto-chiave
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS mto-k-promo = STO-mto-promo, 
           STO-mto-tipo-CF, STO-mto-cod-cli, STO-mto-prg-destino, 
           STO-mto-chiave
           WITH DUPLICATES .
