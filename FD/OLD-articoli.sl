       SELECT OLD-articoli
           ASSIGN       TO  "OLD-articoli"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-OLD-articoli
           RECORD KEY   IS OLD-art-chiave OF OLD-articoli
           ALTERNATE RECORD KEY IS OLD-art-k1 of OLD-articoli = 
           OLD-art-descrizione  OF OLD-articoli
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS OLD-art-codice-ean-1 OF OLD-articoli
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS OLD-art-codice-ean-2 OF OLD-articoli
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS OLD-art-codice-ean-3 OF OLD-articoli
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS OLD-art-codice-ean-4 OF OLD-articoli
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS OLD-art-codice-ean-5 OF OLD-articoli
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS OLD-art-collegato OF OLD-articoli
           WITH DUPLICATES .
