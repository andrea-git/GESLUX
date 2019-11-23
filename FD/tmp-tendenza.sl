       SELECT tmp-tendenza
           ASSIGN       TO  path-tmp-tendenza
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-tmp-tendenza
           RECORD KEY   IS tmdt-chiave.
