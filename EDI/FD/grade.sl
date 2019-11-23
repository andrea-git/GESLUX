      *Tabella monorecord per gli scaglioni
       SELECT grade
           ASSIGN       TO  "GRADE"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-grade
           RECORD KEY   IS gra-chiave.
