      *
      *File delgli agenti
      *E
      *+
      *
      *y
      *+
      *
       SELECT OPTIONAL TBLAG
           ASSIGN       TO RANDOM "TBLAG"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-TBLAG
           RECORD KEY   IS TBLAG-CODICE
           ALTERNATE RECORD KEY IS TBLAG-CODICE-01 = TBLAG-CODICE1, 
           TBLAG-DESCRIZIONE1, TBLAG-CODICE2.
