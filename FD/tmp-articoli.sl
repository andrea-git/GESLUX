      *usato per la ricerca EAN
       SELECT tmp-articoli
           ASSIGN       TO  path-tmp-aricoli
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-tmp-articoli
           RECORD KEY   IS tmp-art-chiave
           ALTERNATE RECORD KEY IS tmp-k-descr = tmp-art-descrizione, 
           tmp-art-codice
           WITH DUPLICATES .
