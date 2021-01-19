      *
      *File delle nazioni
      *b
      *+
      *;
      *r
      *
       SELECT OPTIONAL TBLNA
           ASSIGN       TO RANDOM "TBLNA"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-TBLNA
           RECORD KEY   IS TBLNA-CODICE
           ALTERNATE RECORD KEY IS TBLNA-CODICE-01 = TBLNA-CODICE1, 
           TBLNA-DESCRIZIONE1, TBLNA-CODICE2.
