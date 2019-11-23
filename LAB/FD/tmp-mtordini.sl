      *FILE PER GRIGLIA 1 GIANG --> UTILE AL SOLO FINE DELL'ORDINAMENTO E TPREV PER LO ZOOM SULL'IMPEGNATO MASTER
       SELECT tmp-mtordini
           ASSIGN       TO  path-tmp-mtordini
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-tmp-mtordini
           RECORD KEY   IS tmto-chiave
           ALTERNATE RECORD KEY IS k-ord = tmto-data-cons, tmto-chiave
           WITH DUPLICATES .
