       SELECT tmp-listini
           ASSIGN       TO  path-tmp-listini
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-tmp-listini
           RECORD KEY   IS tlst-chiave
           ALTERNATE RECORD KEY IS tmp-k-articolo = tlst-articolo
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS tmp-k-art-gdo = tlst-gdo, 
           tlst-articolo
           WITH DUPLICATES .
