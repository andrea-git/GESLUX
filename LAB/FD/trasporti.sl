       SELECT trasporti
           ASSIGN       TO  "trasporti"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-trasporti
           RECORD KEY   IS trs-chiave
           ALTERNATE RECORD KEY IS k-data-bolla = trs-data-bolla, 
           trs-num-bolla, trs-prog-bolla
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-data-fattura = trs-data-fattura, 
           trs-data-bolla, trs-num-bolla, trs-prog-bolla
           WITH DUPLICATES .
