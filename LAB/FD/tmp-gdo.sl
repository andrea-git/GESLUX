       SELECT tmp-gdo
           ASSIGN       TO  path-tmp
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-tmp-gdo
           RECORD KEY   IS tgdo-chiave = tgdo-gdo, tgdo-prog.
