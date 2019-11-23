       SELECT tpromo
           ASSIGN       TO  "tpromo"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           LOCK MODE    IS MANUAL WITH ROLLBACK 
           FILE STATUS  IS STATUS-tpromo
           RECORD KEY   IS tpr-chiave OF tpromo
           ALTERNATE RECORD KEY IS tpr-chiave-ricerca = 
           tpr-chiave-ricerca OF tpromo
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS tpr-chiave-gdo-fine = tpr-gdo OF 
           tpromo, tpr-fine-dpo OF tpromo, tpr-ini-dpo OF tpromo
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS tpr-chiave-fine = tpr-fine-dpo OF 
           tpromo, tpr-ini-dpo OF tpromo, tpr-gdo OF tpromo
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS tpr-chiave-ini = tpr-ini-dpo OF 
           tpromo, tpr-fine-dpo OF tpromo, tpr-gdo OF tpromo
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS tpr-chiave-volantino = 
           tpr-ini-volantino OF tpromo, tpr-fine-volantino OF tpromo, 
           tpr-gdo OF tpromo
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS tpr-k-fine-vol = tpr-fine-volantino 
           OF tpromo, tpr-ini-volantino OF tpromo, tpr-gdo OF tpromo
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS tpr-k-data-ins = tpr-data-creazione 
           OF tpromo, tpr-codice OF tpromo, tpr-gdo OF tpromo
           WITH DUPLICATES .
