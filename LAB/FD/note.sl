       SELECT note
           ASSIGN       TO  "note"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-note
           RECORD KEY   IS not-chiave OF note.
