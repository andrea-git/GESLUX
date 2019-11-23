      ** File utilizzato dalle prese per il raggruppamento degli arrivi (solleciti)
       SELECT tmp-arrivi-prese
           ASSIGN       TO  path-tmp-arrivi-prese
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-tmp-arrivi-prese
           RECORD KEY   IS tap-chiave.
