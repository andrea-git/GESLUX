      *"Gruppi di invio"
       SELECT tgruppi
           ASSIGN       TO  "tgruppi"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-tgruppi
           RECORD KEY   IS tgr-chiave.
