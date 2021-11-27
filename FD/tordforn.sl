      ** IN CASO DI MODIFICA ADEGUARE ANCHE TMP-ROF-AUTO
       SELECT tordforn
           ASSIGN       TO  "tordforn"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           LOCK MODE    IS MANUAL
           FILE STATUS  IS STATUS-tordforn
           RECORD KEY   IS tof-chiave
           ALTERNATE RECORD KEY IS tof-k-causale = tof-causale, 
           tof-chiave
           ALTERNATE RECORD KEY IS tof-k-stato = tof-stato, tof-chiave
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-fornitore = tof-cod-forn, 
           tof-destino, tof-stato, tof-chiave
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS tof-k-data = tof-data-ordine OF 
           tordforn, tof-chiave OF tordforn
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS tof-k-consegna = tof-data-consegna 
           OF tordforn, tof-chiave OF tordforn
           WITH DUPLICATES .
