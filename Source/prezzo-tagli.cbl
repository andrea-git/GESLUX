       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      prezzo-tagli.
       AUTHOR.                          Andrea.
  
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "tagli.sl".
           copy "articoli.sl".
           copy "progmag.sl". 
           copy "tmarche.sl".
           copy "timposte.sl".
           copy "ttipocli.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "tagli.fd".
           copy "articoli.fd".
           copy "progmag.fd". 
           copy "tmarche.fd".
           copy "timposte.fd".
           copy "ttipocli.fd".

       WORKING-STORAGE SECTION.
           copy "imposte.def".
           copy "costo-medio.def".

       77  status-tagli      pic xx.
       77  status-articoli   pic xx.
       77  status-progmag    pic xx.
       77  status-tmarche    pic xx.
       77  status-timposte   pic xx.
       77  status-ttipocli   pic xx.

       PROCEDURE DIVISION.
      ***---
       MAIN.
           open input articoli progmag tmarche timposte ttipocli.
           open i-o tagli.

           move low-value to tag-rec.
           start tagli key >= tag-chiave.
           perform until 1 = 2
              read tagli next at end exit perform end-read
              if tag-prz >= 99999
                 |Per i vecchi che non hanno la tipologia
                 if tag-cli-tipo = spaces and tag-gdo not = spaces
                    set TrattamentoGDO to true
                 else
                    move tag-cli-tipo to tcl-codice
                    read ttipocli no lock
                    if ttipocli-gdo
                       set TrattamentoGDO to true
                    else
                       set TrattamentoGDO to false
                    end-if
                 end-if
                 initialize prg-chiave
                 move tag-articolo to prg-cod-articolo art-codice
                 read articoli no lock
                 read progmag  no lock
                 perform CALCOLA-COSTO-MP-COMPLETO
                 move costo-mp to tag-prz
                 rewrite tag-rec
              end-if
           end-perform.

           close articoli tagli progmag tmarche timposte ttipocli.

           goback.

      ***---
       PARAGRAFO-COPY.
           copy "costo-medio.cpy".
           copy "recupero-anagrafica.cpy".
