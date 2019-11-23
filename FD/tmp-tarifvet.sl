       SELECT tmp-tarifvet
           ASSIGN       TO  path-tmp
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-tmp-tarifvet
           RECORD KEY   IS ttar-chiave.
