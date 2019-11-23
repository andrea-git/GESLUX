       SELECT statsett
           ASSIGN       TO  "statsett"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-statsett
           RECORD KEY   IS sts-chiave OF statsett
           ALTERNATE RECORD KEY IS k-ord = sts-mese OF statsett, 
           sts-tipocli OF statsett, sts-marca OF statsett
           WITH DUPLICATES .
