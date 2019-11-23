       SELECT tgrupgdo
           ASSIGN       TO  "tgrupgdo"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-tgrupgdo
           RECORD KEY   IS gdo-chiave OF tgrupgdo
           ALTERNATE RECORD KEY IS gdo-k-g2 = gdo-codice-G2 OF 
           tgrupgdo, gdo-chiave OF tgrupgdo.
