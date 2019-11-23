      *file contenente le qta da aggiungere al programma ordini prese da csv
       SELECT qta-pordini
           ASSIGN       TO  "QTA-PORDINI"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-qta-pordini
           RECORD KEY   IS qp-chiave.
