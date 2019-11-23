       SELECT coperfab-mag
           ASSIGN       TO  path-coperfab-mag
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-coperfab-mag
           RECORD KEY   IS cpfm-chiave
           ALTERNATE RECORD KEY IS k-forn = cpfm-causale, 
           cpfm-fornitore, cpfm-destino, cpfm-articolo, cpfm-listino
           WITH DUPLICATES .
