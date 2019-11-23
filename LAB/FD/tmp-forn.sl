       SELECT tmp-forn
           ASSIGN       TO  path-tmp-forn
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-tmp-forn
           RECORD KEY   IS tmf-chiave
           ALTERNATE RECORD KEY IS tmf-k-articolo = tmf-articolo, 
           tmf-tipo, tmf-prz-confronto, tmf-listino
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS tmf-k-data = tmf-tipo, tmf-articolo, 
           tmf-data-fine
           WITH DUPLICATES .
