       SELECT useravv
           ASSIGN       TO  "useravv"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-useravv
           RECORD KEY   IS uav-chiave OF useravv
           ALTERNATE RECORD KEY IS uav-k-tipologia of useravv = 
           uav-tipologia OF useravv, uav-utente OF useravv
           WITH DUPLICATES .
