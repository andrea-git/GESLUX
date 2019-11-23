      *Associazioni cliente - progressivi (da forzare in master/evasioni)
       SELECT OLD-cli-prg
           ASSIGN       TO  "OLD-cli-prg"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-OLD-cli-prg
           RECORD KEY   IS OLD-cp-chiave.
