      ** Per visualizzare le evasioni da ordine master sia per righe che per testate
      *1
       SELECT zoom-tor-master
           ASSIGN       TO  path-zoom-tor-master
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-zoom-tor-master
           RECORD KEY   IS ztm-chiave.
