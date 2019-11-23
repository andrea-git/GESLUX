       SELECT articoli
           ASSIGN       TO  "articoli"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-articoli
           RECORD KEY   IS art-chiave OF articoli
           ALTERNATE RECORD KEY IS art-k1 of articoli = art-descrizione 
           OF articoli
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS art-codice-ean-1 OF articoli
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS art-codice-ean-2 OF articoli
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS art-codice-ean-3 OF articoli
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS art-codice-ean-4 OF articoli
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS art-codice-ean-5 OF articoli
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS art-collegato OF articoli
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS art-k-frn of articoli = 
           art-cod-art-frn OF articoli
           WITH DUPLICATES .
