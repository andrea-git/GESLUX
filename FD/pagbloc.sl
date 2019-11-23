      ** Lista dei pagamenti bloccati: non è possibile aggiungere lo stato perchè spono tabelle G2
       SELECT pagbloc
           ASSIGN       TO  "pagbloc"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-pagbloc
           RECORD KEY   IS pgb-chiave.
