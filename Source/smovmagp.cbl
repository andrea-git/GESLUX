       PROGRAM-ID.                      smovmagp.
       AUTHOR.                          Andrea.
      ******************************************************************

       SPECIAL-NAMES. 
           DECIMAL-POINT IS COMMA.

       FILE-CONTROL.
           copy "tivaese.sl".
           copy "tmovmag.sl". 
           copy "rmovmag.sl".
           copy "clienti.sl".
           copy "tcaumag.sl".
           copy "tcodpag.sl".
           copy "articoli.sl".
           copy "lineseq.sl". 
           copy "CLI.sl".

       FILE SECTION.
           copy "tivaese.fd".
           copy "tmovmag.fd". 
           copy "rmovmag.fd".
           copy "clienti.fd".
           copy "tcaumag.fd".
           copy "tcodpag.fd".
           copy "articoli.fd".
           copy "lineseq.fd". 
           copy "CLI.fd".

       WORKING-STORAGE SECTION.
           copy "acucobol.def".
           copy "acugui.def".
           copy "comune.def".
           copy "varsca".
                                                
LUBEXX 77  como-tot-imp            pic s9(9)v99.
       77  totale-documento        pic S9(9)v99.
       77  iva-riga                pic s9(9)v999.
       77  iva-riga-2dec           pic s9(9)v99.
       77  totale-iva              pic s9(9)v99.
       77  totale-imponibile       pic S9(9)v99.
       77  totale-coubat           pic 9(9)v99.
       77  tot-riga                pic S9(9)v99.
       77  totale-peso             pic 9(9)v999.
       77  totale-utf              pic 9(9)v999.
       77  como-peso               pic 9(9)v999.
       77  iva                     pic 9(9)v999.
       77  iva-2dec                pic 9(9)v99.
       77  tot-iva                 pic 9(9)v99.
       77  totale-imposta-cons     pic S9(9)v99.
       77  como-qta                pic 9(8).

       77  status-articoli         pic xx.
       77  status-tcaumag          pic xx.
       77  status-tmovmag          pic xx.
       77  status-rmovmag          pic xx.
       77  status-clienti          pic xx.
       77  status-CLI              pic xx.
       77  status-tcodpag          pic xx.
       77  status-lineseq          pic xx.
       77  status-tivaese          pic xx.

       77  wstampa                 pic x(256).
       77  como-data               pic 9(8).
       77  como-ora                pic 9(8).
       77  counter                 pic 9(10).
       77  counter2                pic 9(10).
       77  counter-edit            pic z(10).

LUBEXX 01  tab-iva.
LUBEXX     05 el-iva               occurs 5.
LUBEXX        10 iva-aliquota      pic 9(3)v99.
LUBEXX        10 imponibile-iva    pic 9(12)v99.

       77 TipoIva                   pic  x.
           88 IvaArticoli           value "A".
           88 IvaCliente            value "C".

       77 TipoDiVariazione          pic  x.
           88 VariazioneDiValore    value "V".
           88 VariazioneDiQuantita  value "Q".

       77  filler                  pic x.
           88  Primo-passaggio     value "S", false "N".

       78  titolo                  value 
                                   "Stampa Movimenti di Magazzino".
       01  riga-cliente-causale.
           05 filler               pic x(3) value spaces.
           05 riga-CF              pic x(11).
              88 riga-cliente      value "Cliente   :".
              88 riga-fornitore    value "Fornitore :".
           05 filler               pic x(4).
           05 riga-cli-codice      pic z(5).
           05 filler               pic x.
           05 riga-cli-descr       pic x(40).
           05 filler               pic x(15).
           05 riga-caus-codice     pic x(4).
           05 filler               pic x.
           05 riga-caus-descr      pic x(40).

       01  riga-bolla-reg.
           05 filler               pic x(3) value spaces.
           05 riga-bolla.
              10 filler            pic X(11) value "Bolla n.  :".
              10 filler            pic X.
              10 riga-num-bolla    pic z(8)  blank zero.
              10 filler            pic X     value spaces.
           05 riga-del.
              10 filler            pic X(4)  value "del ".
              10 riga-data-del-1   pic X(8).
              10 filler            pic X(43) value spaces.
           05 riga-reg.
              10 filler            pic X(8)  value "Reg. n. ".
              10 filler            pic X     value ":".
              10 riga-reg-n        pic Z(8).
              10 filler            pic X(5)  value spaces.
              10 filler            pic x(3)  value "del".
              10 filler            pic x     value space.
              10 riga-data-del-2   pic X(8).

       01  riga-pagam-scad.
           05 filler               pic x(3) value spaces.
           05 riga-pagam.
              10 filler            pic X(11)  value "Pagamento :".
              10 filler            pic x.
              10 riga-bonifico     pic X(40).
              10 filler            pic X(24) value spaces.
           05 riga-scad.
              10 filler            pic X(9)  value "Scadenza:".
              10 filler            pic X(1)  value space.
              10 scad-num          pic x(2). 
              10 scad-hyphen       pic x(2).
              10 scad-data         pic X(10).
      *
       01  riga-scad-2.
           05 filler               pic x(89).
           05 scad-num-2           pic x(2).
           05 scad-hyphen-2        pic x(2).
           05 scad-data-2          pic X(10).

       01  riga-peso-utf.
           05 filler               pic x(3)  value spaces.
           05 fller                pic X(16) value "Peso UTF  : Kg. ".
           05 riga-peso            pic zz.zzz.zzz.zz9,999.
           05 filler               pic x(52).
           05 riga-scad-3.
              10 scad-num-3           pic x(2).
              10 scad-hyphen-3        pic x(2).
              10 scad-data-3          pic X(10).

      * CORPO
      *    INTESTAZIONE DELLA "GRIGLIA" PARTE ALTA.
       01  riga-heading-1. 
           05 filler               pic x(3)  value spaces.
           05 filler               pic x(2)  value "Rg".
           05 filler               pic x(2)  value spaces.
           05 filler               pic X(4)  value "Cod.".
           05 filler               pic X(1)  value spaces.
           05 filler               pic X(11) value "Descrizione".
           05 filler               pic X(25) value spaces. 
           05 filler               pic X(3)  value "Nr.".
           05 filler               pic X(8)  value spaces. 
           05 filler               pic X(4)  value "Peso". 
           05 filler               pic X(1)  value spaces. 
           05 filler               pic X(3)  value "Imb". 
           05 filler               pic X(3)  value spaces. 
           05 filler               pic X(10) value "Imponibile".
           05 filler               pic X(5)  value spaces.
           05 filler               pic X(10) value "Imponibile".
           05 filler               pic X(7)  value spaces.
           05 filler               pic X(7)  value "Imposta".
           05 filler               pic X(7)  value spaces.
           05 filler               pic X(4)  value "Cou/".
           05 filler               pic X(7)  value spaces.
           05 filler               pic X(6)  value "Totale".
           05 filler               pic X(16) value spaces.
           05 filler               pic X(2)  value "KG".

      * INTESTAZIONE DELLA "GRIGLIA" PARTE ALTA.
       01  riga-heading-2. 
           05 filler               pic x(3)  value spaces.
           05 filler               pic X(86) value spaces.
           05 filler               pic X(6)  value "Totale".
           05 filler               pic x(7)  value spaces.
           05 filler               pic X(7)  value "Consumo".
           05 filler               pic X(6)  value spaces.
           05 filler               pic x(5)  value "Cobat".
           05 filler               pic x(25) value spaces.
           05 filler               pic X(6)  value "Totali".

      * RIGA PER IL CORPO
       01  riga-corpo.
           05 filler               pic x(2)  value spaces.
           05 riga-dett-rg         pic Z(3).
           05 filler               pic X(1)  value spaces.
           05 riga-dett-cod        pic Z(5).
           05 filler               pic X(1)  value spaces.
           05 riga-dett-descr      pic X(30).
           05 filler               pic X(2)  value spaces.  
           05 riga-dett-nr         pic zzz.zz9 blank zero.
           05 filler               pic X(2)  value spaces.
           05 riga-peso-nr         pic zz.zz9,999 blank zero.
           05 filler               pic X(1)  value spaces.
           05 riga-imb             pic X(3)  value spaces.
           05 filler               pic X(1)  value spaces.
           05 riga-dett-imponibile pic z.zzz.zz9,99.
           05 filler               pic X(2)  value spaces.
LUBEXX     05 riga-dett-tot-imp    pic zz.zzz.zz9,99.
           05 filler               pic X(4)  value spaces.
           05 riga-dett-imposta    pic zzz.zz9,99.
           05 filler               pic X(1)  value spaces.
           05 riga-dett-cou        pic zzz.zz9,99.
           05 filler               pic X(1)  value spaces.
           05 riga-dett-totale     pic z.zzz.zz9,99.
           05 filler               pic X(1)  value spaces.
           05 riga-dett-KG         pic z.zzz.zzz.zz9,999.

      * RIGHE DEL PIEDE
       01  riga-fill-2.
           05 filler               pic x(3)  value spaces.
           05 filler               pic X(37) value spaces.
           05 riga-fill-mezza      pic X(111) value all "-".

       01  riga-totali.
           05 filler               pic x(3)  value spaces.
           05 filler               pic X(37) value SPACES.
           05 filler               pic X(6)  value "TOTALI".
           05 filler               pic X(35) value spaces.
           05 riga-totale-imp      pic zzz.zzz.zz9,99.
           05 filler               pic X(4)  value spaces.
           05 riga-tot-imp-cons    pic zzz.zz9,99.
           05 filler               pic X(1)  value spaces.
           05 riga-tot-cou         pic zzz.zz9,99.
           05 filler               pic X(1)  value spaces.
           05 riga-totale-totale   pic z.zzz.zz9,99.
           05 filler               pic X(1)  value spaces.
           05 riga-tot-KG          pic z.zzz.zzz.zz9,999.

       01  riga-compl.
           05 filler               pic x(3)  value spaces.
           05 filler               pic X(15) value "Kg COMPLESSIVI:".
           05 riga-kg-totali       pic zz.zzz.zzz.zz9,999.
           05 filler               pic X(14) value "  di cui UTF:".
           05 riga-KG-utf          pic zz.zzz.zzz.zz9,999.

       01  riga-tot-euro.
           05 filler               pic x(3)  value spaces.
           05 filler               pic X(16) value "TOTALE Euro   : ".
           05 riga-totale-euro     pic zzz.zzz.zz9,99.
           05 filler               pic x(2).
           05 filler               pic x(8) value "+ Iva = ".
           05 filler               pic x(11).
           05 riga-iva             pic zzz.zzz.zz9,99.

      * SEPARATORE
       01  riga-trattini.
           05 filler               pic x(3)   value spaces.
           05 riga-fill            pic X(148) value all "-".

       77  num-pagina              pic 999.
       77  totale-riga             pic S9(12)v99 value 0.

       78  max-righe        value 43.
      * 77  max-righe-x      pic x(3).
       77  num-righe        pic 99 value 0.
       77  diff-righe       pic 99 value 0.
       77  n-vuote          pic 99 value 0.
       77  sav-riga         pic x(900).

       LINKAGE SECTION.
       copy "smovmagp-limiti.lks".

      ******************************************************************
       PROCEDURE DIVISION USING smovmagp-linkage.

       DECLARATIVES.
       TMOVMAG-ERR SECTION.
           use after error procedure on tmovmag.
           set tutto-ok  to true.
           evaluate status-tmovmag
           when "35"
                display message "Impossibile procedere."
                  x"0d0a""File delle testate [TMOVMAG] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                display message "File [TMOVMAG] Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message "[TMOVMAG] Indexed file corrupt!"
                          title titolo
                           icon 3
                set errori to true
           end-evaluate.  
      
      ***---
       RMOVMAG-ERR SECTION.
           use after error procedure on rmovmag.
           set tutto-ok  to true.
           evaluate status-rmovmag
           when "35"
                display message box       "Impossibile procedere."
                  x"0d0a""File delle righe [RMOVMAG] inesistente"
                        title = titolo
                        icon MB-WARNING-ICON
                set errori to true
           when "39"
                display message "File [RMOVMAG] Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message "[RMOVMAG] Indexed file corrupt!"
                          title titolo
                           icon 3
                set errori to true
           end-evaluate.  
      
      ***---
       TIVAESE-ERR SECTION.
           use after error procedure on tivaese.
           set tutto-ok  to true.
           evaluate status-tivaese
           when "35"
                display message "Impossibile procedere."
                  x"0d0a""File codici IVA [TIVAESE] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                display message "File [TIVAESE] Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message "[TIVAESE] Indexed file corrupt!"
                          title titolo
                           icon 3
                set errori to true
           end-evaluate.  
      
      ***---
       CLIENTI-ERR SECTION.
           use after error procedure on clienti.
           set tutto-ok  to true.
           evaluate status-clienti
           when "35"
                display message "Impossibile procedere."
                  x"0d0a""File clienti [CLIENTI] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                display message "File [CLIENTI] Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message "[CLIENTI] Indexed file corrupt!"
                          title titolo
                           icon 3
                set errori to true
           end-evaluate.  
      
      ***---
       CLI-ERR SECTION.
           use after error procedure on CLI.
           set tutto-ok  to true.
           evaluate status-cli
           when "35"
                display message "Impossibile procedere."
                  x"0d0a""File clienti [CLI] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                display message "File [CLI] Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message "[CLI] Indexed file corrupt!"
                          title titolo
                           icon 3
                set errori to true
           end-evaluate.  
      
      ***---
       TCAUMAG-ERR SECTION.
           use after error procedure on tcaumag.
           set tutto-ok  to true.
           evaluate status-tcaumag
           when "35"
                display message "Impossibile procedere."
                x"0d0a""File causali di magazzino [TCAUMAG] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                display message "File [TCAUMAG] Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message "[TCAUMAG] Indexed file corrupt!"
                          title titolo
                           icon 3
                set errori to true
           end-evaluate.  
      
      ***---
       TCODPAG-ERR SECTION.
           use after error procedure on tcodpag.
           set tutto-ok  to true.
           evaluate status-tcodpag
           when "35"
                display message "Impossibile procedere."
                  x"0d0a""File codici pagamento [TCODPAG] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                display message "File [TCODPAG] Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message "[TCODPAG] Indexed file corrupt!"
                          title titolo
                           icon 3
                set errori to true
           end-evaluate.  
      
      ***---
       ARTICOLI-ERR SECTION.
           use after error procedure on articoli.
           set tutto-ok  to true.
           evaluate status-articoli
           when "35"
                display message "Impossibile procedere."
                  x"0d0a""File articoi [ARTICOLI] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                display message "File [ARTICOLI] Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message "[ARTICOLI] Indexed file corrupt!"
                          title titolo
                           icon 3
                set errori to true
           end-evaluate.  
      
      ***---
       LINESEQ-ERR SECTION.
           use after error procedure on lineseq.
           set tutto-ok  to true.
           evaluate status-lineseq
           when "35"
                display message "Impossibile procedere."
                  x"0d0a""File TXT inesistente"
                          title titolo
                           icon 2
                set errori to true
           end-evaluate.
       END DECLARATIVES.

      ***---
       MAIN-PRG.
           perform INIT.
           perform OPEN-FILES.

           if tutto-ok
              perform ELABORAZIONE
              perform EXIT-PGM
           end-if.

      ***---
       INIT.
      *     accept  max-righe-x   from environment "NUM-MAX-RIGHE-ORIZ".
      *     move    max-righe-x   to max-righe with convert.
           set     tutto-ok            to true.
           set     primo-passaggio     to true.

           move 0 to num-righe.
           move 1 to num-pagina.

           accept  wstampa       from environment "PATH-ST".
           accept  como-data     from century-date.
           accept  como-ora      from time.
           inspect wstampa       replacing trailing spaces by low-value.
      
           string  wstampa       delimited low-value
                   "smovmag"     delimited size
                   "_"           delimited size
                   como-data     delimited size
                   "_"           delimited size
                   como-ora      delimited size
                   ".txt"        delimited size
              into wstampa
           end-string.

           inspect wstampa      replacing trailing low-values by spaces.

      ***---
       OPEN-FILES.
           open input  tmovmag, rmovmag, clienti,
                       tcodpag, tcaumag, articoli,
                       tivaese, CLI.

           open output lineseq.
           write line-riga from space after 1.

      ***---
       ELABORAZIONE.
           set tutto-ok to true.
           set trovato  to false.

           move smovmagp-anno   to tmo-anno.
           move smovmagp-caus   to tmo-causale.
           move smovmagp-da-num to tmo-numero.

           if tmo-causale not = spaces
              start tmovmag key is >= key01
                    invalid set errori to true
              end-start
           else
              start tmovmag key is >= tmo-chiave
                    invalid set errori to true
              end-start
           end-if.

           if tutto-ok
              perform until 1 = 2
                 read tmovmag next no lock at end exit perform end-read

                 if tmo-anno    not = smovmagp-anno
                    exit perform
                 end-if

                 if tmo-numero   >    smovmagp-a-num
                    exit perform
                 end-if

                 if smovmagp-caus  not = spaces
                    if tmo-causale not = smovmagp-caus
                       exit perform
                    end-if
                 end-if

                 add 1 to counter
                 add 1 to counter2
                 if counter2 = 50
                    move counter to counter-edit
                    display counter-edit
                       upon smovmagp-handle at column 01,00
                                                 line 10,00
                    move 0 to counter2
                 end-if

                 perform RELAZIONE-TMOVMAG-CLIENTI
                 perform RELAZIONE-CLIENTI-TIVAESE
                 perform RELAZIONE-TMOVMAG-CAUSALI
                 perform RELAZIONE-TMOVMAG-TCODPAG
                 perform CALCOLA-TOTALE-MOVIM

                 move tmo-anno   to rmo-anno
                 move tmo-numero to rmo-movim
                 move low-values to rmo-riga
                 perform STAMPA-TESTATA

                 start rmovmag key >= rmo-chiave
                       invalid set errori to true
                 end-start

                 if tutto-ok
                    perform until 1 = 2
                       read rmovmag next at end exit perform end-read

                       if rmo-anno    not = tmo-anno   or
                          rmo-movim   not = tmo-numero
                          exit perform
                       end-if

                       perform STAMPA-CORPO
                    end-perform

                    perform STAMPA-PIEDE
                 end-if

              end-perform
           end-if.

           if not trovato
              display message "Nessun documento presente avente"
                              " il criterio selezionato"
                        title titolo
              close lineseq
              delete file lineseq
      *     else
      *        perform SALTO-PAGINA
           end-if.       

           display "                           " upon smovmagp-handle
                                            at column 01,00
                                            at line   10,00.

      ***---
       CALCOLA-TOTALE-MOVIM.
LUBEXX     initialize tab-iva replacing numeric data by zeroes
LUBEXX                             alphanumeric data by spaces.
           move 0          to totale-riga totale-documento totale-iva.
           move tmo-anno   to rmo-anno.
           move tmo-numero to rmo-movim.
           move low-values to rmo-riga.
           start rmovmag key is >= rmo-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read rmovmag next at end exit perform end-read
                    if rmo-anno  not = tmo-anno  or
                       rmo-movim not = tmo-numero
                       exit perform
                    end-if
LUBEXX              if rmo-qta = 0 
LUBEXX                 move 1 to como-qta 
LUBEXX              else
LUBEXX                 move rmo-qta to como-qta
LUBEXX              end-if
                    compute totale-riga  =
                         (( rmo-netto    +
                            rmo-imp-cons +
                            rmo-coubat ) * como-qta )
                    compute totale-documento =  totale-riga +
                                                totale-documento

LUBEXX              if cli-iva-ese = spaces
LUBEXX                 move rmo-articolo to art-codice
LUBEXX                 read articoli no lock invalid continue end-read
LUBEXX                 move "IV"           to tbliv-codice1
LUBEXX                 move art-codice-iva to tbliv-codice2
LUBEXX                 read tivaese no lock 
LUBEXX                      invalid move 0 to tbliv-percentuale
LUBEXX                 end-read
LUBEXX                 set IvaArticoli to true
LUBEXX                 perform SOMMA-IMPONIBILI-IVA
LUBEXX              else
LUBEXX                 set IvaCliente  to true
LUBEXX              end-if

                 end-perform

LUBEXX           move 0 to totale-iva

LUBEXX           if IvaCliente
LUBEXX              compute iva =
LUBEXX                    ( totale-documento * tbliv-percentuale) / 100
LUBEXX              add 0,005 to iva 
LUBEXX              move iva  to iva-2dec
LUBEXX              move iva-2dec to totale-iva
LUBEXX           else
LUBEXX              perform varying idx from 1 by 1
LUBEXX                        until idx > 5
LUBEXX                 compute iva = 
LUBEXX                      (( imponibile-iva(idx) * 
LUBEXX                         iva-aliquota(idx) ) / 100 )
LUBEXX                 add 0,005 to iva 
LUBEXX                 move iva to iva-2dec
LUBEXX                 compute totale-iva = 
LUBEXX                         totale-iva + iva-2dec
LUBEXX              end-perform
LUBEXX           end-if

                 compute totale-documento = 
                         totale-documento + totale-iva

                 initialize variabili-varsca 
                            replacing numeric data by zeroes
                                 alphanumeric data by spaces
                 move tmo-codpag        to sca-codice-pa
                 move tmo-data-doc      to sca-data-fattura
                 move tmo-data-doc      to sca-data-conteggio
                 move totale-documento  to sca-importo-fattura
                 move totale-documento  to sca-importo-fattura-va
                 move totale-iva        to sca-iva
                 move totale-iva        to sca-iva-va
           
                 move cli-mese1         to sca-mese1
                 move cli-giorno1       to sca-giorno1
                 move cli-mese2         to sca-mese2
                 move cli-giorno2       to sca-giorno2

                 move cli-escluso-dal-giorno1 to sca-escluso-dal-giorno1
                 move cli-escluso-dal-giorno2 to sca-escluso-dal-giorno2

                 call   "calsca" using variabili-varsca
                 cancel "calsca"

           end-start.

LUBEXX***---
LUBEXX SOMMA-IMPONIBILI-IVA.
LUBEXX     move 0 to idx.
LUBEXX     perform varying idx from 1 by 1 
LUBEXX               until idx > 5
LUBEXX        if iva-aliquota(idx) = 0 or
LUBEXX           iva-aliquota(idx) = tbliv-percentuale
LUBEXX           exit perform
LUBEXX        end-if
LUBEXX     end-perform.
LUBEXX     add totale-riga        to imponibile-iva(idx).
LUBEXX     move tbliv-percentuale to iva-aliquota(idx).

      ***---
       STAMPA-TESTATA.
           if primo-passaggio
              set trovato              to true
              set primo-passaggio      to false
           else
              perform SALTO-PAGINA
           end-if.

      * INIZIALIZZO I TOTALIZZATORI.
           initialize totale-imponibile,   
                      totale-imposta-cons, 
                      totale-coubat,       
                      tot-riga,            
                      totale-peso,         
                      totale-utf.

      * STAMPA IL TITOLO
      *     move smovmagp-anno   to riga-anno.
      *     move smovmagp-da-num to riga-da-num.
      *     move smovmagp-a-num  to riga-a-num.


      * STAMPA LA RIGA DEL CLIENTE E CAUSALE.
           move  cli-codice   to riga-cli-codice.
           move  tca-codice   to riga-caus-codice.

           if tmo-cliente     set riga-cliente   to true
           else               set riga-fornitore to true
           end-if.

           initialize line-riga.
           move riga-cliente-causale to line-riga.
           perform STAMPA-RIGA.

      * STAMPA LA RIGA BOLLA E DATA
           move  tmo-numdoc-clifor   to riga-num-bolla.
           move  all "/"             to riga-data-del-1.
           move  tmo-data-doc(7:2)   to riga-data-del-1(1:2).
           move  tmo-data-doc(5:2)   to riga-data-del-1(4:2).
           move  tmo-data-doc(3:4)   to riga-data-del-1(7:2).

           move  tmo-numero          to riga-reg-n.
           move  all "/"             to riga-data-del-2.
           move  tmo-data-movim(7:2) to riga-data-del-2(1:2).
           move  tmo-data-movim(5:2) to riga-data-del-2(4:2).
           move  tmo-data-movim(3:4) to riga-data-del-2(7:2).

           move spaces to scad-num    scad-num-2    scad-num-3.
           move spaces to scad-hyphen scad-hyphen-2 scad-hyphen-3.
           move spaces to scad-data   scad-data-2   scad-data-3.

           if sca-importo(1) not = 0
              move " 1"      to scad-num
              |move sca-importo(1)       to scad-importo
              move ") "                 to scad-hyphen
              |move " - "                to scad-div
              if sca-a-vista(1) = "S"
                 |move " (A VISTA) ;"    to scad-end
                 move tmo-data-doc(7:2) to scad-data(1:2)
                 move "/"               to scad-data(3:1)
                 move tmo-data-doc(5:2) to scad-data(4:2)
                 move "/"               to scad-data(6:1)
                 move tmo-data-doc(1:4) to scad-data(7:4)
              else
                 move gg of sca-data(1) to scad-data(1:2)
                 move "/"               to scad-data(3:1)
                 move mm of sca-data(1) to scad-data(4:2)
                 move "/"               to scad-data(6:1)
                 move aa of sca-data(1) to scad-data(7:4)
                 |move " ;"              to scad-end
              end-if
           end-if.
      *
           initialize line-riga.
           move riga-bolla-reg to line-riga.
           perform STAMPA-RIGA.
      *
      ** STAMPA DATI DEL PAGAMENTO
           initialize line-riga.
           move riga-pagam-scad to line-riga.
           perform STAMPA-RIGA.
      *
           if sca-importo(2) not = 0
              move " 2"      to scad-num-2
              |move sca-importo(2)       to scad-importo-2
              move ") "                 to scad-hyphen-2
              |move " - "                to scad-div-2
              if sca-a-vista(2) = "S"
                 |move " (A VISTA) ;"    to scad-end-2
                 move tmo-data-doc(7:2) to scad-data-2(1:2)
                 move "/"               to scad-data-2(3:1)
                 move tmo-data-doc(5:2) to scad-data-2(4:2)
                 move "/"               to scad-data-2(6:1)
                 move tmo-data-doc(1:4) to scad-data-2(7:4)
              else
                 move gg of sca-data(2) to scad-data-2(1:2)
                 move "/"               to scad-data-2(3:1)
                 move mm of sca-data(2) to scad-data-2(4:2)
                 move "/"               to scad-data-2(6:1)
                 move aa of sca-data(2) to scad-data-2(7:4)
                 |move " ;"              to scad-end-2
              end-if
           end-if.

           initialize line-riga.

           move riga-scad-2 to line-riga.
           perform STAMPA-RIGA.
      *
      ** STAMPA IL PESO UTF
           move tmo-peso-utf    to riga-peso.
      *
           if sca-importo(3) not = 0
              move " 3"      to scad-num-3
              |move sca-importo(3)       to scad-importo-3
              move ") "                 to scad-hyphen-3
              |move " - "                to scad-div-3
              if sca-a-vista(3) = "S"
                 |move " (A VISTA) ;"    to scad-end-3
                 move tmo-data-doc(7:2) to scad-data-3(1:2)
                 move "/"               to scad-data-3(3:1)
                 move tmo-data-doc(5:2) to scad-data-3(4:2)
                 move "/"               to scad-data-3(6:1)
                 move tmo-data-doc(1:4) to scad-data-3(7:4)
              else
                 move gg of sca-data(3) to scad-data-3(1:2)
                 move "/"               to scad-data-3(3:1)
                 move mm of sca-data(3) to scad-data-3(4:2)
                 move "/"               to scad-data-3(6:1)
                 move aa of sca-data(3) to scad-data-3(7:4)
                 |move " ;"              to scad-end-3
              end-if
           end-if.

           initialize line-riga.
           move riga-peso-utf to line-riga.
           perform STAMPA-RIGA.

           move spaces to line-riga.
           perform STAMPA-RIGA.

           initialize line-riga.
           move riga-trattini to line-riga.
           perform STAMPA-RIGA.

           initialize line-riga.
           move riga-heading-1 to line-riga.
           perform STAMPA-RIGA.

           initialize line-riga.
           move riga-heading-2 to line-riga.
           perform STAMPA-RIGA.

           initialize line-riga.
           move riga-trattini to line-riga.
           perform STAMPA-RIGA.

           move spaces to line-riga.
           perform STAMPA-RIGA.
          
      ***---
       STAMPA-CORPO.
      * STAMPA LE RIGHE DEL CORPO.
           move rmo-riga        to riga-dett-rg.
           move rmo-articolo    to riga-dett-cod.
           
           perform RELAZIONE-RMOVMAG-ARTICOLI.

           move art-descrizione to riga-dett-descr.
           move rmo-qta         to riga-dett-nr.
           move rmo-peso        to riga-peso-nr.
           move rmo-imballo     to riga-imb.
           move rmo-netto       to riga-dett-imponibile.

           call "C$JUSTIFY"  using riga-dett-imponibile, "R".

LUBEXX     compute como-tot-imp = rmo-qta * rmo-netto.
LUBEXX     move como-tot-imp    to riga-dett-tot-imp.
LUBEXX     call "C$JUSTIFY"  using riga-dett-tot-imp, "R".

           move rmo-imp-cons    to riga-dett-imposta.
           move rmo-coubat      to riga-dett-cou.

           call "C$JUSTIFY"  using riga-dett-imposta,    "R".
           call "C$JUSTIFY"  using riga-dett-cou,        "R".
           
           perform CALCOLA-TOTALI-PIEDE.
           
           move totale-riga     to riga-dett-totale.

           add rmo-peso-tot to rmo-peso-tot-utf giving como-peso.
           move como-peso   to riga-dett-KG.

           call "C$JUSTIFY"  using riga-dett-KG,         "R".

           initialize line-riga.
           move riga-corpo to line-riga.
           perform STAMPA-RIGA.

      ***---
       STAMPA-PIEDE.
      * STAMPA I TOTALI IN FONDO E IL RESTO DEL PIEDE.
           move spaces to line-riga.
           perform STAMPA-RIGA.

           initialize line-riga.
           move riga-fill-2 to line-riga.
           perform STAMPA-RIGA.

           move  totale-imponibile   to riga-totale-imp
           move  totale-imposta-cons to riga-tot-imp-cons.
           move  totale-coubat       to riga-tot-cou.
           move  tot-riga            to riga-totale-totale.
           move  totale-peso         to riga-tot-KG.           

           initialize line-riga.
           move riga-totali to line-riga.
           perform STAMPA-RIGA.

           initialize line-riga.
           move riga-trattini to line-riga.
           perform STAMPA-RIGA.

      * TOTALI COMPLESSIVI (RESTO DEL PIEDE).
           move  totale-peso         to riga-kg-totali.
           move  totale-utf          to riga-kg-utf.

           move spaces to line-riga.
           perform STAMPA-RIGA.

           initialize line-riga.
           move riga-compl to line-riga.
           perform STAMPA-RIGA.

           move spaces to line-riga.
           perform STAMPA-RIGA.
                                                     
           move  tot-riga            to riga-totale-euro.
           
           |compute iva = ((tot-riga * tbliv-percentuale) / 100).

           |add 0,005 to iva.
           |move  iva to iva-2dec.
           |move  iva-2dec            to tot-iva.

           |add   tot-iva             to tot-riga.

           |move  tot-riga            to riga-iva.
           move totale-documento to riga-iva.

           initialize line-riga.
           move riga-tot-euro to line-riga.
           perform STAMPA-RIGA.
           move 0 to tot-iva.

      ***---
       CALCOLA-TOTALI-PIEDE.
      * SONO I TOTALI IN FONDO ALLA "GRIGLIA".
           if rmo-qta = 0 
              move 1 to como-qta
           else
              move rmo-qta to como-qta
           end-if.

           compute totale-imponibile   =
                   totale-imponibile   + (rmo-netto    * como-qta).
           compute totale-imposta-cons =
                   totale-imposta-cons + (rmo-imp-cons * como-qta).
           compute totale-coubat       =
                   totale-coubat       + (rmo-coubat   * como-qta).
           compute totale-riga = 
                  (rmo-netto + rmo-imp-cons  + rmo-coubat) * como-qta.

           compute totale-peso = totale-peso  +
                               ( rmo-peso-tot +
                                 rmo-peso-tot-utf ).
           
           add   totale-riga        to tot-riga.

           compute totale-utf = totale-utf + rmo-peso-tot-utf.

      ***---
       RELAZIONE-TMOVMAG-CLIENTI.
      * RECUPERO DESCRIZIONI DAI FILE IN RELAZIONE 
           evaluate true
           when tmo-cliente    set cli-tipo-C to true
           when tmo-fornitore  set cli-tipo-F to true
           end-evaluate.

           move tmo-cod-clifor to cli-codice.

           read clienti   key cli-chiave
              invalid     move "* NON TROVATO *" to riga-cli-descr
              not invalid move cli-ragsoc-1      to riga-cli-descr
           end-read.

           move cli-codice to cli-codice-G2.
           read CLI no lock invalid continue end-read.

      ***---
       RELAZIONE-TMOVMAG-CAUSALI.
      * RECUPERO DESCRIZIONI DAI FILE IN RELAZIONE 
           move tmo-causale    to tca-codice.

           set VariazioneDiQuantita to true.
           read tcaumag key tca-chiave
                invalid move "* NON TROVATO *"  to riga-caus-descr
            not invalid move tca-descrizione    to riga-caus-descr
                        if tca-no-movim-giac and 
                           tca-no-movim-imp  and
                           tca-no-movim-ord  and
                           tca-no-giac-bloc
                           set VariazioneDiValore   to true
                        else
                           set VariazioneDiQuantita to true
                        end-if
           end-read.

      ***---
       RELAZIONE-TMOVMAG-TCODPAG.
      * RECUPERO DESCRIZIONI DAI FILE IN RELAZIONE 
           move "PA"           to tblpa-codice1.
           move tmo-codpag     to tblpa-codice2.

           read tcodpag no lock
                invalid move "* NON TROVATO *"  to riga-bonifico
            not invalid 
                initialize riga-bonifico
                inspect tblpa-descrizione1 replacing trailing 
                                              spaces by low-value
                string  tblpa-descrizione1 delimited by low-value
                        " "                delimited by size
                        tblpa-descrizione2 delimited by size
                        into riga-bonifico
                end-string
           end-read.             

      ***---
       RELAZIONE-CLIENTI-TIVAESE.
           move "IV"        to tbliv-codice1.
           move cli-iva-ese to tbliv-codice2.
           read tivaese no lock
                invalid move 0 to tbliv-percentuale
           end-read.

      ***---
       RELAZIONE-RMOVMAG-ARTICOLI.
           move rmo-articolo             to art-codice.

           read articoli key art-chiave
                invalid  move "* NON TROVATO *"  to riga-dett-descr
            not invalid  
                initialize riga-bonifico
                inspect tblpa-descrizione1 replacing trailing 
                                              spaces by low-value
                string  tblpa-descrizione1 delimited by low-value
                        " "                delimited by size
                        tblpa-descrizione2 delimited by size
                        into riga-bonifico
                end-string
           end-read.

      ***---
       EXIT-PGM.
           close lineseq, clienti, tmovmag, 
                 rmovmag, tcodpag, tcaumag
                 tivaese, CLI.

           move wstampa to smovmagp-path.

           goback.

      ***---
       SALTO-PAGINA.
      *     compute diff-righe = max-righe - num-righe.
      *     move diff-righe to n-vuote
      *     perform RIGHE-VUOTE  
           move 0 to num-righe.
           add 1 to num-pagina.
           write line-riga from spaces after page
      *    luciano
           write line-riga from x"09" after 1.

      ***---
       RIGHE-VUOTE.
           perform n-vuote times
              write line-riga from spaces
           end-perform
           add n-vuote to num-righe. 

      ***---
       STAMPA-RIGA.
           initialize sav-riga
           move line-riga to sav-riga
           if num-righe > max-righe - 5
              perform SALTO-PAGINA
           end-if
           move sav-riga to line-riga
           write line-riga
           add 1 to num-righe.         
