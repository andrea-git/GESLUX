       SELECT tmp-sol
           ASSIGN       TO  path-tmp-solleciti-m
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS status-tmp-solleciti-m
           RECORD KEY   IS sol-chiave OF tmp-sol
           ALTERNATE RECORD KEY IS k-dt-testa = sol-tipo OF tmp-sol, 
           sol-data-ordine OF tmp-sol, sol-ordine OF tmp-sol
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-art = sol-tipo OF tmp-sol, 
           sol-ordine OF tmp-sol
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-eva = sol-tipo OF tmp-sol, 
           sol-ordine OF tmp-sol, sol-prog-art OF tmp-sol, sol-prog-eva 
           OF tmp-sol
           WITH DUPLICATES .
