      *log macrobatch condiviso dagli applicativi coinvolti
       SELECT log-macrobatch
           ASSIGN       TO  path-log-macrobatch
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS  IS STATUS-log-macrobatch.
