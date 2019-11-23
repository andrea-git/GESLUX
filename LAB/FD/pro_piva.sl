       SELECT pro_piva
           ASSIGN       TO  "pro_piva"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-pro-piva
           RECORD KEY   IS piva-chiave
           ALTERNATE RECORD KEY IS piva-dati
           WITH DUPLICATES .
