      *
      *File delle categorie clienti
      * 
      *
       SELECT OPTIONAL TBLCA
           ASSIGN       TO RANDOM "TBLCA"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-TBLCA
           RECORD KEY   IS TBLCA-CODICE
           ALTERNATE RECORD KEY IS TBLCA-CODICE-01 = TBLCA-CODICE1, 
           TBLCA-DESCR1, TBLCA-CODICE2.
