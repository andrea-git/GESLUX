      ** Usato per ricalimp per immagazzinare i valori dei progressivi da aggiornare
       SELECT tmp-ricalimp
           ASSIGN       TO  path-tmp-ricalimp
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-tmp-ricalimp
           RECORD KEY   IS tric-prg-chiave.
