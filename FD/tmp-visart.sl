       SELECT tmp-visart
           ASSIGN       TO  path-tmp-visart
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-tmp-visart
           RECORD KEY   IS tvis-chiave
           ALTERNATE RECORD KEY IS k-ord = tvis-chiave, 
           tvis-data-fattura, tvis-num-fattura, tvis-cli-ragsoc
           WITH DUPLICATES .
