       SELECT OPTIONAL TBLPC
           ASSIGN       TO RANDOM "TBLPC"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS status-tblpc
           RECORD KEY   IS TBLPC-CODICE
           ALTERNATE RECORD KEY IS TBLPC-CODICE-01 = TBLPC-CODICE1, 
           TBLPC-DESCRIZIONE1, TBLPC-CODICE2.
