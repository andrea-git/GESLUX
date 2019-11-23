       SELECT tcaumag
           ASSIGN       TO  "tcaumag"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-tcaumag
           RECORD KEY   IS tca-chiave
           ALTERNATE RECORD KEY IS k-mag = tca-cod-magaz, tca-ord-forn
           WITH DUPLICATES .
