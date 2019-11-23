      ** IN CASO DI MODIFICA ADEGUARE ANCHE TMP-STO-rof-AUTO
       SELECT STO-rordforn
           ASSIGN       TO  path-sto-rordforn
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           LOCK MODE    IS MANUAL WITH ROLLBACK 
           FILE STATUS  IS STATUS-sto-rordforn
           RECORD KEY   IS STO-rof-chiave OF STO-rordforn
           ALTERNATE RECORD KEY IS STO-rof-k-articolo of STO-rordforn = 
           STO-rof-cod-articolo OF STO-rordforn, STO-rof-chiave OF 
           STO-rordforn
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS STO-rof-k-art-mag of STO-rordforn = 
           STO-rof-cod-articolo OF STO-rordforn, STO-rof-cod-magazzino 
           OF STO-rordforn, STO-rof-chiave OF STO-rordforn
           WITH DUPLICATES .
