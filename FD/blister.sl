       SELECT blister
           ASSIGN       TO  "blister"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-blister
           RECORD KEY   IS bli-chiave
           ALTERNATE RECORD KEY IS k-magaz = bli-magazzino, bli-codice
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-des = bli-descrizione
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS bli-codice-ean-1 OF blister
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS bli-codice-ean-2 OF blister
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS bli-codice-ean-3 OF blister
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS bli-codice-ean-4 OF blister
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS bli-codice-ean-5 OF blister
           WITH DUPLICATES .
