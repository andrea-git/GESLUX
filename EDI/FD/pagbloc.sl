      ** Lista dei pagamenti bloccati: non � possibile aggiungere lo stato perch� spono tabelle G2
       SELECT pagbloc
           ASSIGN       TO  "pagbloc"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-pagbloc
           RECORD KEY   IS pgb-chiave.
