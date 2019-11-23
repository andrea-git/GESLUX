       SELECT tmp-redi
           ASSIGN       TO  path-tmp-redi
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           LOCK MODE    IS MANUAL
           FILE STATUS  IS STATUS-tmp-redi
           RECORD KEY   IS tmp-redi-chiave OF tmp-redi.
