       SELECT redi
           ASSIGN       TO  "redi"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           LOCK MODE    IS MANUAL
           FILE STATUS  IS STATUS-redi
           RECORD KEY   IS redi-chiave OF redi
           ALTERNATE RECORD KEY IS redi-k-tipo-mov of redi = 
           redi-tedi-chiave OF redi, redi-tipo-movim OF redi, 
           redi-chiave-mov OF redi
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS redi-k-dt-mov of redi = 
           redi-tedi-chiave OF redi, redi-tipo-movim OF redi, 
           redi-data-movimento OF redi, redi-tmo-chiave OF redi
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS redi-tmo-chiave OF redi
           WITH DUPLICATES .
