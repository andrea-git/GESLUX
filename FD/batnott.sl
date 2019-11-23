      *Organizzazione BATCH NOTTURNI
       SELECT batnott
           ASSIGN       TO  "batnott"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-batnott
           RECORD KEY   IS bat-chiave
           ALTERNATE RECORD KEY IS bat-k-des = bat-programma, bat-chiave
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS bat-k-abil = bat-abilitato, 
           bat-programma, bat-chiave
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS bat-k-bloc = bat-bloccante, 
           bat-programma, bat-chiave
           WITH DUPLICATES .
