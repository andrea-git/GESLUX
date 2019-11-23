       SELECT rmovmag
           ASSIGN       TO  "rmovmag"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           LOCK MODE    IS MANUAL WITH ROLLBACK 
           FILE STATUS  IS STATUS-rmovmag
           RECORD KEY   IS rmo-chiave OF rmovmag
           ALTERNATE RECORD KEY IS k-articolo of rmovmag = rmo-anno OF 
           rmovmag, rmo-movim OF rmovmag, rmo-articolo OF rmovmag
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS rmo-chiave-ricerca of rmovmag = 
           rmo-tipo OF rmovmag, rmo-cod-clifor OF rmovmag, rmo-causale 
           OF rmovmag, rmo-articolo OF rmovmag, rmo-data-movim OF 
           rmovmag
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-progmag of rmovmag = 
           rmo-chiave-progmag OF rmovmag
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-art-data of rmovmag = rmo-articolo 
           OF rmovmag, rmo-data-movim OF rmovmag
           WITH DUPLICATES .
