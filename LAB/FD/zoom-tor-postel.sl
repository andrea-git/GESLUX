       SELECT zoom-tor-postel
           ASSIGN       TO  path-zoom-tor-postel
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-zoom-tor-postel
           RECORD KEY   IS zoom-top-key01 = zoom-top-chiave
           ALTERNATE RECORD KEY IS zoom-top-numero
           WITH DUPLICATES .
