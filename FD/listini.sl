       SELECT listini
           ASSIGN       TO  "listini"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-listini
           RECORD KEY   IS lst-chiave OF listini
           ALTERNATE RECORD KEY IS lst-k-articolo = lst-gdo OF listini, 
           lst-articolo OF listini, lst-data OF listini
           ALTERNATE RECORD KEY IS lst-k-cod-art-cli = lst-gdo OF 
           listini, lst-cod-art-cli OF listini, lst-data OF listini
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS lst-k-data = lst-data OF listini, 
           lst-gdo OF listini, lst-cod-art-cli OF listini
           WITH DUPLICATES .
