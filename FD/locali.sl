       SELECT locali
           ASSIGN       TO  "locali"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-locali
           RECORD KEY   IS loc-chiave
           ALTERNATE RECORD KEY IS loc-chiave-ricerca
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS loc-chiave-gdo-fine = loc-gdo, 
           loc-cliente, loc-destino, loc-fine-dpo, loc-ini-dpo
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS loc-chiave-fine = loc-fine-dpo, 
           loc-ini-dpo, loc-gdo, loc-cliente, loc-destino
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS loc-chiave-ini = loc-ini-dpo, 
           loc-fine-dpo, loc-gdo, loc-cliente, loc-destino
           WITH DUPLICATES .
