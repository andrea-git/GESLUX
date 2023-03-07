      ***---
       INIT.
      *-
           move 1 to num-page.
           accept como-data from century-date.
           accept como-ora  from time.
           initialize wstampa.
           move 0 to num-righe.
           move 0 to tot-marche.
           set trovata-marca  to false.
           set tutto-ok       to true.
           set trovato        to false.
           set prima-volta    to true.
           accept num-max-righe-x from environment "NUM_MAX_RIGHE_ORIZ".
           move num-max-righe-x to max-righe with convert.
           accept  wstampa    from environment "PATH_ST".
           inspect wstampa    replacing trailing spaces by low-value.
           if RicalcoloNotturno                           
              move tge-data-consolid-progmag(5:2) to como-anno
              if como-anno = 12
                 move tge-data-consolid-progmag(1:4) to como-anno
                 add 1 to como-anno
              else
                 move tge-data-consolid-progmag(1:4) to como-anno
              end-if
              accept  wstampa     from environment "PATH_STAT"        
              inspect wstampa     replacing trailing spaces by low-value
              |Uno per giorno altrimenti non lo trovo più
              string  wstampa     delimited low-value
                      "statsett-" delimited size
                      como-anno   delimited size
                      "_"         delimited size
                      link-mese   delimited size
                      ".txt"      delimited size
                 into wstampa
              end-string       

           else
              string  wstampa    delimited low-value
                      "statsett" delimited size
                      "_"        delimited size
                      como-data  delimited size
                      "_"        delimited size
                      como-ora   delimited size
                      ".txt"     delimited size
                 into wstampa
              end-string
           end-if.

      ***---
       OPEN-FILES.
           perform OPEN-OUTPUT-LINESEQ.
           if tutto-ok
              open input statsett
              if tutto-ok
                 open input tmarche ttipocli
                 if errori
                    close statsett
                    close lineseq
                    delete file lineseq
                 end-if
              else
                 close lineseq
                 delete file lineseq
              end-if
           end-if.

      ***---
       OPEN-OUTPUT-LINESEQ.
           open output lineseq.

      ***---
       VALORIZZA-OCCURS.
           if tot-marche = 0
              move 1 to tot-marche
           else
              set trovata-marca to false
              set idx           to 1
              search marche-occurs
              when codice-marca(idx)   = sts-marca   and
                   codice-tipocli(idx) = sts-tipocli and
                   codice-mese(idx)    = sts-mese
                   set trovata-marca to true
                   move idx to tot-marche
              end-search
              if not trovata-marca
                 add 1 to tot-marche                 
              end-if
           end-if.
           compute como-valore =
                 ( sts-fat-corr - sts-csm-corr ) + sts-adeguam-corr.
           add     como-valore to  el-tot-resa(tot-marche).
           move    sts-marca   to codice-marca(tot-marche).
           move    sts-tipocli to codice-tipocli(tot-marche).
           move    sts-mese    to codice-mese(tot-marche).

      ***---
       SCRIVI-RIGA.
           move    sts-marca       to mar-codice.
           read tmarche no lock invalid continue end-read.
           move    mar-descrizione to r-descr.

           compute como-valore rounded =
                  |Come richiesta di Mori (22/11/2007): bisogna
                  |adeguare anche il margine/Kg
                 ( sts-fat-corr - sts-csm-corr + 
                   sts-adeguam-corr) / sts-kg-corr.
LUBEXX     move como-valore to margine-corr.
           move como-valore to r-mrg-kg-1.

           compute como-valore rounded = 
                               sts-kg-corr / 100.
           move    como-valore to r-qli-1.
           add     como-valore to tot-mese-qli-1.
           add     como-valore to tot-periodo-qli-1.
LUBEXX     if statraff-mensile
LUBEXX        add     como-valore to tot-qli-1
LUBEXX     end-if.

           move    sts-fat-corr  to r-fatt-1.
           add     sts-fat-corr  to tot-mese-fatt-1.
           add     sts-fat-corr  to tot-periodo-fatt-1.
LUBEXX     if statraff-mensile
LUBEXX        add     sts-fat-corr  to tot-fatt-1
LUBEXX     end-if.

           compute como-valore = 
                 ( sts-fat-corr - sts-csm-corr ) + sts-adeguam-corr.
           move    como-valore   to r-resa-1.
           add     como-valore   to tot-mese-resa-1.
           add     como-valore   to tot-periodo-resa-1.
LUBEXX     if statraff-mensile
LUBEXX        add     como-valore   to tot-resa-1
LUBEXX     end-if.

      ***********************************************************

           compute como-valore rounded =
                  |Come richiesta di Mori (22/11/2007): bisogna
                  |adeguare anche il margine/Kg 
                 ( sts-fat-past - sts-csm-past +
                   sts-adeguam-past ) / sts-kg-past.
LUBEXX     move como-valore to margine-prec.
           move como-valore to r-mrg-kg-2.     

           compute como-valore rounded = 
                                  sts-kg-past / 100.
           move    como-valore to r-qli-2.
           add     como-valore to tot-mese-qli-2.
           add     como-valore to tot-periodo-qli-2.
LUBEXX     if statraff-mensile
LUBEXX        add     como-valore to tot-qli-2
LUBEXX     end-if.

           move    sts-fat-past  to r-fatt-2.
           add     sts-fat-past  to tot-mese-fatt-2.
           add     sts-fat-past  to tot-periodo-fatt-2.
LUBEXX     if statraff-mensile
LUBEXX        add     sts-fat-past  to tot-fatt-2
LUBEXX     end-if.

           compute como-valore = 
                 ( sts-fat-past - sts-csm-past ) + sts-adeguam-past.
           move    como-valore   to r-resa-2.
           add     como-valore   to tot-mese-resa-2.
           add     como-valore   to tot-periodo-resa-2.
LUBEXX     if statraff-mensile
LUBEXX        add     como-valore   to tot-resa-2
LUBEXX     end-if.

      *************************************************************

LUBEXX*****           compute como-valore rounded =
LUBEXX*****                 (( sts-fat-corr - sts-csm-corr ) / sts-kg-corr) -
LUBEXX*****                 (( sts-fat-past - sts-csm-past ) / sts-kg-past).
LUBEXX*****           move como-valore to r-mrg-kg-3.

      * Come da richiesta di Trivella (08/09/06) il margine di
      * scostamento non va ricalcolato ma va fatta lo sottrazione
LUBEXX     compute como-valore = margine-corr - margine-prec.
LUBEXX     move    como-valore to r-mrg-kg-3.

           compute como-valore rounded =
                                ( sts-kg-corr / 100) -
                                ( sts-kg-past / 100).
           move    como-valore to r-qli-3.
           add     como-valore to tot-mese-qli-3.
           add     como-valore to tot-periodo-qli-3.
LUBEXX     if statraff-mensile
LUBEXX        add     como-valore to tot-qli-3
LUBEXX     end-if.

           compute como-valore = sts-fat-corr - sts-fat-past.
           move    como-valore to r-fatt-3.
           add     como-valore to tot-mese-fatt-3.
           add     como-valore to tot-periodo-fatt-3.
LUBEXX     if statraff-mensile
LUBEXX        add     como-valore to tot-fatt-3
LUBEXX     end-if.

           compute como-valore =
               ( ( sts-fat-corr - sts-csm-corr ) + sts-adeguam-corr ) -
               ( ( sts-fat-past - sts-csm-past ) + sts-adeguam-past ).
           move    como-valore   to r-resa-3.
           add     como-valore   to tot-mese-resa-3.
           add     como-valore   to tot-periodo-resa-3.
LUBEXX     if statraff-mensile
LUBEXX        add     como-valore   to tot-resa-3
LUBEXX     end-if.

           initialize line-riga.
           move riga to line-riga.
           perform STAMPA-RIGA.
           set trovato       to true.

      ***---
       SCRIVI-TOTALI-MESE.
           if num-righe > max-righe - 4
              perform SALTO-PAGINA
           end-if.
           initialize line-riga.
           move riga-div-2 to line-riga.
           perform STAMPA-RIGA.

           evaluate SaveMese
           when  1 move "GENNAIO"   to tm-mese
           when  2 move "FEBBRAIO"  to tm-mese
           when  3 move "MARZO"     to tm-mese
           when  4 move "APRILE"    to tm-mese
           when  5 move "MAGGIO"    to tm-mese
           when  6 move "GIUGNO"    to tm-mese
           when  7 move "LUGLIO"    to tm-mese
           when  8 move "AGOSTO"    to tm-mese
           when  9 move "SETTEMBRE" to tm-mese
           when 10 move "OTTOBRE"   to tm-mese
           when 11 move "NOVEMBRE"  to tm-mese
           when 12 move "DICEMBRE"  to tm-mese
           end-evaluate.

           compute tot-mese-marg-1 rounded =
                   tot-mese-resa-1 / ( tot-mese-qli-1 * 100 ).
                                     
           compute tot-mese-marg-2 rounded =
                   tot-mese-resa-2 / ( tot-mese-qli-2 * 100 ).

      * Come da richiesta di Trivella (08/09/06) il margine di
      * scostamento non va ricalcolato ma va fatta lo sottrazione
LUBEXX*****           compute tot-mese-marg-3 rounded =
LUBEXX*****                   tot-mese-resa-3 / ( tot-mese-qli-3 * 100 ).

LUBEXX     compute tot-mese-marg-3 rounded =
LUBEXX             tot-mese-marg-1 - tot-mese-marg-2.

           move tot-mese-qli-1      to tm-qli-1.
           move tot-mese-marg-1     to tm-mrg-kg-1.
           move tot-mese-resa-1     to tm-resa-1.
           move tot-mese-fatt-1     to tm-fatt-1.

           move tot-mese-qli-2      to tm-qli-2.
           move tot-mese-marg-2     to tm-mrg-kg-2.
           move tot-mese-resa-2     to tm-resa-2.
           move tot-mese-fatt-2     to tm-fatt-2.

           move tot-mese-qli-3      to tm-qli-3.
           move tot-mese-marg-3     to tm-mrg-kg-3.
           move tot-mese-resa-3     to tm-resa-3.
           move tot-mese-fatt-3     to tm-fatt-3.

           initialize line-riga.
           move riga-tot-mese to line-riga.
           perform STAMPA-RIGA.

           initialize line-riga.
           move riga-div-2 to line-riga.
           perform STAMPA-RIGA.

           move spaces to line-riga.
           perform STAMPA-RIGA.

LUBEXX*****Essendo una stampa cumulata i totali GENERALI devono essere
LUBEXX*****recuperati dai progressivi, perciò per una stampa che va da
LUBEXX*****Gennaio a Febbraio dovrò prendere i progressivi di Febbraio
LUBEXX*****che durante l'aggiornamento vengono valorizzati come GEN + FEB
LUBEXX     if statraff-cumulato
LUBEXX        if SaveMese = statraff-mese-a
LUBEXX           add tot-mese-qli-1   to tot-qli-1
LUBEXX           add tot-mese-resa-1  to tot-resa-1
LUBEXX           add tot-mese-fatt-1  to tot-fatt-1
LUBEXX           add tot-mese-qli-2   to tot-qli-2
LUBEXX           add tot-mese-resa-2  to tot-resa-2
LUBEXX           add tot-mese-fatt-2  to tot-fatt-2
LUBEXX           add tot-mese-qli-3   to tot-qli-3
LUBEXX           add tot-mese-resa-3  to tot-resa-3
LUBEXX           add tot-mese-fatt-3  to tot-fatt-3
LUBEXX        end-if
LUBEXX     end-if.

           move 0 to tot-mese-qli-1 
                     tot-mese-marg-1
                     tot-mese-resa-1
                     tot-mese-fatt-1
                     tot-mese-qli-2 
                     tot-mese-marg-2
                     tot-mese-resa-2
                     tot-mese-fatt-2
                     tot-mese-qli-3 
                     tot-mese-marg-3
                     tot-mese-resa-3
                     tot-mese-fatt-3.

      ***---
       SCRIVI-TOTALI-PERIODO.
           if num-righe > max-righe - 4
              perform SALTO-PAGINA
           end-if.

           compute tot-periodo-marg-1 rounded =
                   tot-periodo-resa-1 / (tot-periodo-qli-1 * 100).

           compute tot-periodo-marg-2 rounded =
                   tot-periodo-resa-2 / (tot-periodo-qli-2 * 100).
                   
      * Come da richiesta di Trivella (08/09/06) il margine di
      * scostamento non va ricalcolato ma va fatta lo sottrazione

LUBEXX*****           compute tot-periodo-marg-3 rounded =
LUBEXX*****                   tot-periodo-resa-3 / (tot-periodo-qli-3 * 100).
                   
           compute tot-periodo-marg-3 rounded =
                   tot-periodo-marg-1 - tot-periodo-marg-2.

           move tot-periodo-qli-1  to tp-qli-1.
           move tot-periodo-marg-1 to tp-mrg-kg-1.
           move tot-periodo-resa-1 to tp-resa-1.
           move tot-periodo-fatt-1 to tp-fatt-1.

           move tot-periodo-qli-2  to tp-qli-2.
           move tot-periodo-marg-2 to tp-mrg-kg-2.
           move tot-periodo-resa-2 to tp-resa-2.
           move tot-periodo-fatt-2 to tp-fatt-2.

           move tot-periodo-qli-3  to tp-qli-3.
           move tot-periodo-marg-3 to tp-mrg-kg-3.
           move tot-periodo-resa-3 to tp-resa-3.
           move tot-periodo-fatt-3 to tp-fatt-3.
                  
           initialize line-riga.
           move riga-div-1 to line-riga.
           perform STAMPA-RIGA.

           initialize line-riga.
           move riga-tot-periodo   to line-riga.
           perform STAMPA-RIGA.

           initialize line-riga.
           move riga-div-1 to line-riga.
           perform STAMPA-RIGA.

           move spaces to line-riga.
           perform STAMPA-RIGA.

           move 0 to tot-periodo-qli-1
                     tot-periodo-marg-1
                     tot-periodo-resa-1
                     tot-periodo-fatt-1
                     tot-periodo-qli-2
                     tot-periodo-marg-2
                     tot-periodo-resa-2
                     tot-periodo-fatt-2
                     tot-periodo-qli-3
                     tot-periodo-marg-3
                     tot-periodo-resa-3
                     tot-periodo-fatt-3.

      ***---
       SCRIVI-TOTALI-GENERALI.
           if num-righe > max-righe - 4
              perform SALTO-PAGINA
           end-if.

           compute tot-marg-1 rounded =
                   tot-resa-1 / (tot-qli-1 * 100).

           compute tot-marg-2 rounded =
                   tot-resa-2 / (tot-qli-2 * 100).

      * Come da richiesta di Trivella (08/09/06) il margine di
      * scostamento non va ricalcolato ma va fatta lo sottrazione

LUBEXX*****           compute tot-marg-3 rounded =
LUBEXX*****                   tot-resa-3 / (tot-qli-3 * 100).

LUBEXX     compute tot-marg-3 rounded =
LUBEXX             tot-marg-1 - tot-marg-2.

           move tot-qli-1  to t-qli-1.
           move tot-marg-1 to t-mrg-kg-1.
           move tot-resa-1 to t-resa-1.
           move tot-fatt-1 to t-fatt-1.

           move tot-qli-2  to t-qli-2.
           move tot-marg-2 to t-mrg-kg-2.
           move tot-resa-2 to t-resa-2.
           move tot-fatt-2 to t-fatt-2.

           move tot-qli-3  to t-qli-3.
           move tot-marg-3 to t-mrg-kg-3.
           move tot-resa-3 to t-resa-3.
           move tot-fatt-3 to t-fatt-3.

           initialize line-riga.
           move riga-totali to line-riga.
           perform STAMPA-RIGA.
           
           initialize line-riga.           
           move riga-div-2 to line-riga.
           perform STAMPA-RIGA.

           move 0 to tot-qli-1 
                     tot-marg-1
                     tot-resa-1
                     tot-fatt-1
                     tot-qli-2 
                     tot-marg-2
                     tot-resa-2
                     tot-fatt-2
                     tot-qli-3 
                     tot-marg-3
                     tot-resa-3
                     tot-fatt-3.
                     
      ***---
       SALTO-PAGINA.
           move 0 to num-righe.
           write line-riga from space after page.
           write line-riga from x"09" after 1.
           add 1 to num-page.
           move num-page  to tit-page.
           move riga-page to line-riga.
           write line-riga.
           add 1 to num-righe.

      ***---
       STAMPA-RIGA.
           initialize sav-riga
           move line-riga to sav-riga
           if num-righe > max-righe| - 5
              perform SALTO-PAGINA
           end-if.
           move sav-riga to line-riga
           write line-riga
           add 1 to num-righe. 

      ***---
       SCRIVI-INTESTAZIONE-COMMON.
           if prima-volta
              move num-page  to tit-page
              move riga-page to line-riga
              write line-riga
              add 1 to num-righe
           end-if.
           if num-righe > max-righe - 4
              perform SALTO-PAGINA
           end-if.
           initialize line-riga.
           move riga-div-1 to line-riga.
           perform STAMPA-RIGA.

           move SaveTipo to tcl-codice.     
           read ttipocli no lock invalid continue end-read.
           move tcl-descrizione to tit-tipologia.

           
           if tcl-descrizione = "Carburanti" and mar-codice = 1
           stop "K" end-if

           initialize line-riga.
           move titolo-1 to line-riga.
           perform STAMPA-RIGA.

           initialize line-riga.
           move riga-div-3 to line-riga.
           perform STAMPA-RIGA.

           initialize line-riga.
           move titolo-2   to line-riga.
           perform STAMPA-RIGA.

           initialize line-riga.
           move riga-div-3 to line-riga.
           perform STAMPA-RIGA.

      ***---
       CLOSE-FILES.
           close lineseq statsett tmarche ttipocli.
           if not trovato
              delete file lineseq
           end-if.
