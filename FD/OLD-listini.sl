       SELECT OLD-listini
           ASSIGN       TO  "OLD-listini"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-OLD-listini
           RECORD KEY   IS OLD-lst-chiave
           ALTERNATE RECORD KEY IS OLD-lst-k-articolo = OLD-lst-gdo, 
           OLD-lst-articolo, OLD-lst-data.
