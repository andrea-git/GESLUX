      *
      *"
      *R
      *9
       SELECT OPTIONAL TBLCO
           ASSIGN       TO RANDOM "TBLCO"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-TBLCO
           RECORD KEY   IS TBLCO-CODICE
           ALTERNATE RECORD KEY IS TBLCO-CODICE-01 = TBLCO-CODICE1, 
           TBLCO-DESCRIZIONE1, TBLCO-CODICE2.
