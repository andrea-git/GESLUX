       SELECT lisagente
           ASSIGN       TO  "lisagente"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-lisagente
           RECORD KEY   IS lis-chiave OF lisagente
           ALTERNATE RECORD KEY IS k-codice = lis-codice OF lisagente
           WITH DUPLICATES .
