      ** IN CASO DI MODIFICA ADEGUARE ANCHE TMP-ROF-AUTO
       SELECT STO-tordforn
           ASSIGN       TO  path-sto-tordforn
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           LOCK MODE    IS MANUAL
           FILE STATUS  IS STATUS-sto-tordforn
           RECORD KEY   IS STO-tof-chiave
           ALTERNATE RECORD KEY IS STO-tof-k-causale = STO-tof-causale, 
           STO-tof-chiave
           ALTERNATE RECORD KEY IS STO-tof-k-stato = STO-tof-stato, 
           STO-tof-chiave
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-fornitore = STO-tof-cod-forn, 
           STO-tof-destino, STO-tof-stato, STO-tof-chiave
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS STO-tof-k-data = STO-tof-data-ordine 
           OF STO-tordforn, STO-tof-chiave OF STO-tordforn
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS STO-tof-k-consegna = 
           STO-tof-data-consegna, STO-tof-chiave
           WITH DUPLICATES .
