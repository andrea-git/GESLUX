      *DEcodifica codici IVA per EDI
       SELECT EDI-tiva
           ASSIGN       TO  "EDI-tiva"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-EDI-tiva
           RECORD KEY   IS eiv-chiave
           ALTERNATE RECORD KEY IS eiv-collegato
           WITH DUPLICATES .
