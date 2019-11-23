      ** Tmp usato da evasione e giang per contenere il MINOR numero di imballi EVADIBILI
       SELECT tmp-blis-eva
           ASSIGN       TO  path-tmp-blis-eva
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-tmp-blis-eva
           RECORD KEY   IS tblis-chiave.
