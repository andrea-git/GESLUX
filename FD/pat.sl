       SELECT OPTIONAL PAT
           ASSIGN       TO DISK 
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           LOCK MODE    IS MANUAL WITH ROLLBACK 
           FILE STATUS  IS status-pat
           RECORD KEY   IS Key-0 = pat-codice of record-pat
           ALTERNATE RECORD KEY IS key-1 = pat-codice-conto of 
           record-pat, pat-data-riferimento of record-pat, 
           pat-numero-riferimento of record-pat, pat-codice of 
           record-pat
           ALTERNATE RECORD KEY IS key-2 = pat-data-riferimento of 
           record-pat, pat-codice-conto of record-pat, pat-codice of 
           record-pat.
