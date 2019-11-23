      *Associazioni cliente - progressivi (da forzare in master/evasioni)
       SELECT cli-prg
           ASSIGN       TO  "cli-prg"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-cli-prg
           RECORD KEY   IS cp-chiave.
