      *
      *
      *
       SELECT OPTIONAL TBLME
           ASSIGN       TO RANDOM "TBLME"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-TBLME
           RECORD KEY   IS TBLMe-CODICE
           ALTERNATE RECORD KEY IS TBLME-CODICE-01 = TBLME-CODICE1, 
           TBLME-DESCRIZIONE1, TBLME-CODICE2.
