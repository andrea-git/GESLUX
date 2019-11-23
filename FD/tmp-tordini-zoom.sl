       SELECT tmp-tordini-zoom
           ASSIGN       TO  path-tmp-tordini-zoom
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-tmp-tordini-zoom
           RECORD KEY   IS tmp-tor-key01 = tmp-tor-z-chiave
           ALTERNATE RECORD KEY IS tmp-tord-bolla
           WITH DUPLICATES .
