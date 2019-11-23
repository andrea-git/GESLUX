       SELECT tmp-cerca-bozze
           ASSIGN       TO  path-tmp-cerca-bozze
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS status-tmp-cerca-bozze
           RECORD KEY   IS tmp-b-chiave
           ALTERNATE RECORD KEY IS k-reso = tmp-b-anno, tmp-b-num-reso
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-bolla = tmp-b-bolla
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-fattura = tmp-b-fattura
           WITH DUPLICATES .
