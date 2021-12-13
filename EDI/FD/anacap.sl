       SELECT anacap
           ASSIGN       TO  "anacap"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-anacap
           RECORD KEY   IS anc-chiave
           ALTERNATE RECORD KEY IS k-prov = anc-prov, anc-comune, 
           anc-chiave
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-comune = anc-comune, anc-prov, 
           anc-chiave
           WITH DUPLICATES .
