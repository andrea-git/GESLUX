      ** File a indici che tiene traccia dei contatori creati in G2 al momento della fatturazione
       SELECT contab
           ASSIGN       TO  "contab"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-contab
           RECORD KEY   IS contab-chiave.
