       SELECT battsost
           ASSIGN       TO  "battsost"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS status-battsost
           RECORD KEY   IS bts-chiave OF battsost
           ALTERNATE RECORD KEY IS bts-art-princ of battsost = 
           bts-princ OF battsost, bts-codice OF battsost
           WITH DUPLICATES .
