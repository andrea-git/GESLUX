       SELECT tmp-solleciti-m
           ASSIGN       TO  path-tmp-solleciti-m
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS status-tmp-solleciti-m
           RECORD KEY   IS sol-chiave
           ALTERNATE RECORD KEY IS k-testa = sol-tipo, sol-ordine
           WITH DUPLICATES .
