       SELECT tmp-solleciti
           ASSIGN       TO  path-tmp-solleciti
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS status-tmp-solleciti
           RECORD KEY   IS k-evasione = tmp-sol-evasione
           ALTERNATE RECORD KEY IS k-bolla = tmp-sol-bolla
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-fatt = tmp-sol-fatt
           WITH DUPLICATES .
