      *
      *
      *
      **
       SELECT OPTIONAL TBLCS
           ASSIGN       TO RANDOM "TBLCS"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-TBLCS
           RECORD KEY   IS TBLCS-CODICE
           ALTERNATE RECORD KEY IS TBLCS-CODICE-01 = TBLCS-CODICE1, 
           TBLCS-DESCR1, TBLCS-CODICE2.
