      ** IN CASO DI MODIFICA ADEGUARE ANCHE TMP-OLD-STO-rof-AUTO
       SELECT OLD-STO-rordforn
           ASSIGN       TO  path-OLD-STO-rordforn
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           LOCK MODE    IS MANUAL WITH ROLLBACK 
           FILE STATUS  IS STATUS-OLD-STO-rordforn
           RECORD KEY   IS OLD-STO-rof-chiave OF OLD-STO-rordforn
           ALTERNATE RECORD KEY IS 
           OLD-STO-rof-k-articolo of OLD-STO-rordforn = 
           OLD-STO-rof-cod-articolo OF OLD-STO-rordforn, 
           OLD-STO-rof-chiave OF 
           OLD-STO-rordforn
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS 
           OLD-STO-rof-k-art-mag of OLD-STO-rordforn = 
           OLD-STO-rof-cod-articolo OF OLD-STO-rordforn, 
           OLD-STO-rof-cod-magazzino 
           OF OLD-STO-rordforn, OLD-STO-rof-chiave OF OLD-STO-rordforn
           WITH DUPLICATES .
