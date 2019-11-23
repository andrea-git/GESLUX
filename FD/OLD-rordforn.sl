      ** IN CASO DI MODIFICA ADEGUARE ANCHE TMP-OLD-rof-AUTO
       SELECT OLD-rordforn
           ASSIGN       TO  "OLD-rordforn"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           LOCK MODE    IS MANUAL WITH ROLLBACK 
           FILE STATUS  IS STATUS-OLD-rordforn
           RECORD KEY   IS OLD-rof-chiave OF OLD-rordforn
           ALTERNATE RECORD KEY IS OLD-rof-k-articolo of OLD-rordforn = 
           OLD-rof-cod-articolo OF OLD-rordforn, 
           OLD-rof-chiave OF OLD-rordforn
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS OLD-rof-k-art-mag of OLD-rordforn = 
           OLD-rof-cod-articolo OF OLD-rordforn, 
           OLD-rof-cod-magazzino OF OLD-rordforn, 
           OLD-rof-chiave OF OLD-rordforn
           WITH DUPLICATES .
