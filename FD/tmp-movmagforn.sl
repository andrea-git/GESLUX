       SELECT tmp-movmagforn
           ASSIGN       TO  path-tmp-movmagforn
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-tmp-movmagforn
           RECORD KEY   IS tmp-movf-chiave
           ALTERNATE RECORD KEY IS tmp-movf-k1 = tmp-movf-codice, 
           tmp-movf-articolo, tmp-movf-data-movim
           WITH DUPLICATES .
