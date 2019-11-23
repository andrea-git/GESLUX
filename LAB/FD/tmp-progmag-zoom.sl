       SELECT tmp-progmag-zoom
           ASSIGN       TO  path-tmp-progmag-zoom
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-tmp-progmag-zoom
           RECORD KEY   IS key-des = tmp-prg-z-art-des, tmp-prg-z-chiave
           ALTERNATE RECORD KEY IS key-art = tmp-prg-z-cod-articolo
           WITH DUPLICATES .
