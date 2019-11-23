      *Settaggi invio mail
       SELECT tsetinvio
           ASSIGN       TO  "tsetinvio"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-tsetinvio
           RECORD KEY   IS tsi-chiave.
