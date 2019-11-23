      ** IN CASO DI MODIFICA ADEGUARE ANCHE TMP-ROF-AUTO
       SELECT rordforn
           ASSIGN       TO  "rordforn"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           LOCK MODE    IS MANUAL WITH ROLLBACK 
           FILE STATUS  IS STATUS-rordforn
           RECORD KEY   IS rof-chiave OF rordforn
           ALTERNATE RECORD KEY IS rof-k-articolo of rordforn = 
           rof-cod-articolo OF rordforn, rof-chiave OF rordforn
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS rof-k-art-mag of rordforn = 
           rof-cod-articolo OF rordforn, rof-cod-magazzino OF rordforn, 
           rof-chiave OF rordforn
           WITH DUPLICATES .
