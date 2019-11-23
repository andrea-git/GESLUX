       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      agg-promo-fittizie.
       AUTHOR.                          Andrea.
       REMARKS. Partendo dalle promo fittizie, elimino tutte le righe e
                le ricreo dalle sole righe NON CHIUSE del master di 
                riferimento. Nel caso il master non esista o sia chiuso
                elimino definitivamente la promo.
                In questo modo elimino le righe, ma restando la testa
                anche la data di creazione (con cui prenoto le quantità)
                rimane invariata. Elimina inoltre le righe senza testa.
  
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "mtordini.sl".
           copy "mrordini.sl".
           copy "tpromo.sl".
           copy "rpromo.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "mtordini.fd".
           copy "mrordini.fd".
           copy "tpromo.fd".
           copy "rpromo.fd".

       WORKING-STORAGE SECTION.

       77  status-mtordini  pic xx.
       77  status-mrordini  pic xx.
       77  status-tpromo    pic xx.
       77  status-rpromo    pic xx.

       01  errori          pic xx.
           88 tutto-ok     value "OK".
           88 errori       value "ER".

       01  filler          pic 9.
           88 record-ok    value 1, false 0.

       01  filler          pic 9.
           88 RecLocked    value 1, false 0.

       PROCEDURE DIVISION.
      ***---
       MAIN.
           open i-o mtordini mrordini tpromo rpromo.

           move 999999 to tpr-codice.
           start tpromo key >= tpr-chiave.
           perform until 1 = 2
              read tpromo next at end exit perform end-read
              move low-value  to rpr-rec
              move tpr-codice to rpr-codice
              move tpr-chiave-master to mto-chiave
              perform ELIMINA-RIGHE-PROMO
              read mtordini no lock
                   invalid  delete tpromo record end-delete
               not invalid  
                   if mto-chiuso delete tpromo record end-delete
                   else          perform CREA-RIGHE-PROMO
                   end-if
              end-read
           end-perform.
           perform ELIMINA-RIGHE-SENZA-TESTA.

           close mtordini mrordini tpromo rpromo.

           goback.

      ***---
       ELIMINA-RIGHE-PROMO.
           move low-value  to rpr-rec.
           move tpr-codice to rpr-codice.
           start rpromo key >= rpr-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read rpromo next at end exit perform end-read
                    if rpr-codice not = tpr-codice
                       exit perform
                    end-if
                    delete rpromo record
                 end-perform
           end-start.

      ***---
       CREA-RIGHE-PROMO.
           move low-value  to mro-rec.
           move mto-chiave to mro-chiave-testa.
           start mrordini key >= mro-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read mrordini next at end exit perform end-read
                    if mro-chiave-testa not = mto-chiave
                       exit perform
                    end-if
                    if mro-chiuso
                       move 0 to mro-promo
                    else
                       move 0                  to rpr-prz-acq
                       move mto-promo-fittizia to rpr-codice
                       move mro-cod-articolo   to rpr-articolo
                       |In caso di articolo doppio
                       read rpromo no lock
                            invalid compute rpr-qta = mro-qta - 
                                                      mro-qta-e
                        not invalid compute rpr-qta = 
                                            rpr-qta + 
                                          ( mro-qta - mro-qta-e )
                       end-read
                       move mro-prz-unitario to rpr-prz-ven
                       accept rpr-data-creazione from century-date
                       accept rpr-ora-creazione  from time
                       move mro-utente-creazione to rpr-utente-creazione
                       move mro-chiave         to rpr-mro-chiave
                       write rpr-rec invalid   rewrite rpr-rec end-write
                       move mto-promo-fittizia to mro-promo
                    end-if
                    rewrite mro-rec
                 end-perform
           end-start. 

      ***---
       ELIMINA-RIGHE-SENZA-TESTA.
           move low-value to rpr-rec.
           move 999999    to rpr-codice.
           start rpromo  key >= rpr-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read rpromo next at end exit perform end-read
                    if rpr-codice < 999999
                       exit perform
                    end-if
                    move rpr-codice to tpr-codice
                    read tpromo no lock
                         invalid delete rpromo record
                    end-read
                 end-perform
           end-start. 
