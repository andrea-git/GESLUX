       SELECT EDI-mrordini
           ASSIGN       TO  "EDI-mrordini"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           LOCK MODE    IS MANUAL
           FILE STATUS  IS STATUS-EDI-mrordini
           RECORD KEY   IS emro-chiave
           ALTERNATE RECORD KEY IS emro-k-articolo = emro-cod-articolo, 
           emro-chiave
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS emro-k-stato = emro-stato, 
           emro-chiave
           WITH DUPLICATES .
