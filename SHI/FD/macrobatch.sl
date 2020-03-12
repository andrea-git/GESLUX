      *Usato dalla procedura macrobatch per passare stati e valori* 
       SELECT macrobatch
           ASSIGN       TO DISK "macrobatch"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-macrobatch
           RECORD KEY   IS mb-chiave.
