       IDENTIFICATION       DIVISION.
       PROGRAM-ID.          sitfin-p.
       AUTHOR.              Andrea.
       REMARKS.
           Calcolo dei dati di controllo situazione finanziaria. 
           Parte di sola elaborazione, sfruttato dal programma di
           "Esposizione" da clienti e da ordini e dalla mail notturna.

       ENVIRONMENT          DIVISION.
       CONFIGURATION        SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY "clienti.sl".
           COPY "rmovmag.sl".
           COPY "tmovmag.sl".
           COPY "tparamge.sl".
           COPY "tcaumag.sl".
           COPY "PAT.sl".
           COPY "PAS.sl".
           copy "tivaese.sl".
           copy "tordini.sl".
           copy "rordini.sl".
           copy "tnotacr.sl".
           copy "rnotacr.sl".
           copy "sitfin.sl".   
           copy "ttipocli.sl".
           copy "grade.sl".

       DATA DIVISION.
       FILE SECTION.
           COPY "clienti.fd".
           COPY "rmovmag.fd".
           COPY "tmovmag.fd".
           COPY "tparamge.fd".
           COPY "tcaumag.fd".
           COPY "PAT.fd".
           COPY "PAS.fd".
           copy "tivaese.fd".
           copy "tordini.fd".
           copy "rordini.fd".
           copy "tnotacr.fd".
           copy "rnotacr.fd".
           copy "sitfin.fd".  
           copy "ttipocli.fd".
           copy "grade.fd".

       WORKING-STORAGE SECTION.
           copy "recupero-addizionale.def".

       78  titolo     value "Controllo situazione finanaziaria".

       77  status-clienti   pic x(2).
       77  status-rmovmag   pic x(2).
       77  status-tmovmag   pic x(2).
       77  status-tparamge  pic x(2).
       77  status-tcaumag   pic x(2).
       77  status-pas       pic x(2).
       77  status-pat       pic x(2).
       77  status-tivaese   pic x(2).
       77  status-tordini   pic x(2).
       77  status-rordini   pic x(2).
       77  status-tnotacr   pic x(2).
       77  status-rnotacr   pic x(2).   
       77  status-sitfin    pic x(2).
       77  status-ttipocli  pic x(2).
       77  status-grade     pic x(2).

       01  filler           pic xx.
           88 tutto-ok      value "OK".
           88 errori        value "ER".
                                      
       77  mese-rif         pic 99.
       77  como-anno        pic 9999.
       77  perce-max-z      pic ---9,99.
       77  perce            pic s9(3)v99.
       77  num              pic s9(9)v99.   
       77  den              pic s9(9)v99.
       77  totale           pic s9(13)v99.     
       77  fido-tmp         pic s9(13)v99.
       77  Sum              pic s9(13)v99.
       77  tot-fido         pic s9(13)v99.
       01  data-2mesi.                      
           05 anno          pic 9999.
           05 mese-2        pic 99.
           05 giorno        pic 99.
       77  fatturato        pic s9(9)v99.
       77  como-valore      pic s9(9)v99.
       77  scoperto-tot     pic s9(9)v99.
       77  como-data        pic 9(8).
       77  como-iva         pic 9(12)v999.
       77  como-iva-2dec    pic 9(12)v99.
       77  idx              pic 9.
       01  tab-iva.
           05 el-imponib    pic 9(6)v99 occurs 3.
           05 el-perce-iva  pic 9(3)v99 occurs 3.

       77  como-giorno      pic 9(4).
       77  cont             pic 9(4).

       LINKAGE SECTION.
           copy  "link-sitfin-p.def".
           copy  "link-calfido.def".

       PROCEDURE DIVISION USING sitfin-p-linkage, 
                                calfido-linkage.

      ***---
       MAIN-PROGRAM.
           perform INIT.
           perform OPEN-FILES.
           perform ELABORAZIONE.
           perform CLOSE-FILES.
           perform EXIT-PGM.

      ***---
       INIT.
           set tutto-ok to true.

      ***---
       OPEN-FILES.
           open input clienti.
           open input rmovmag.
           open input tmovmag.
           open input tparamge.
           open input tcaumag.
           open input PAT.
           open input PAS.
           open input tivaese.
           open input tordini.
           open input rordini.
           open input tnotacr.
           open input rnotacr.
           open input ttipocli.
           open input grade.
           open i-o   sitfin.

      ***---
       ELABORAZIONE.
           move link-cli-codice to cli-codice.
           set cli-tipo-C to true.
           read clienti no lock invalid continue end-read.


           perform CALCOLO-FIDO.
           if calfido-status = 0        
              move cli-codice to link-cli-codice
              perform VERIFICA-1
              perform VERIFICA-2
      *****     perform VERIFICA-3.

              move sitfin-p-scaduto-al to sf-scaduto-al
              move sitfin-p-scadenze   to sf-scadenze
              move sitfin-p-perce-max  to sf-perce-max
              move sitfin-p-fat-per    to sf-fat-per

              rewrite sf-rec invalid continue end-rewrite
           end-if.

      ***---
       CALCOLO-FIDO.
           move spaces to tge-chiave.
           read tparamge no lock invalid continue end-read.

           call   "calfido"  using calfido-linkage.
           cancel "calfido".

      ***---
       VERIFICA-1.  
           compute scoperto-tot = saldo-scaduto   +
                                  saldo-scadenza  +
                                  effetti-rischio +
                                  ordini-in-essere.

           move cli-piva to sf-piva.

           read sitfin  no lock
                invalid initialize sf-saldo-scaduto
                        initialize sf-saldo-scadenza
                        initialize sf-effetti-rischio
                        initialize sf-ordini-in-essere
           end-read.

           if cli-fido not = 0  or sf-ragsoc = spaces
              move cli-ragsoc-1 to sf-ragsoc
              move cli-codice   to sf-cod-cli
           end-if.

           add saldo-scaduto           to sf-saldo-scaduto.
           add saldo-scadenza          to sf-saldo-scadenza.
           add effetti-rischio         to sf-effetti-rischio.
           add ordini-in-essere        to sf-ordini-in-essere. 
           

           move cli-tipo to tcl-codice.
           read ttipocli.
           compute totale = saldo           +
                            effetti-rischio +
                            ordini-in-essere.

           if cli-gestione-fido-si       
              if tcl-fido-nuovo-si            
                 compute fido-tmp = cli-fido
              else
                 compute tot-fido = cli-fido
              end-if
           else
              if tcl-fido-nuovo-si
                 compute fido-tmp = cli-fido
              else
                 compute tot-fido = cli-fido + 
                                    cli-pfa  + 
                                    cli-fidejussione
              end-if
           end-if.
           if tcl-fido-nuovo-si  
              if cli-fidejussione > 0

                 if cli-grade > 0
                    move spaces to gra-codice
                    read grade no lock
                         invalid move 0 to Sum
                     not invalid
                         perform varying idx from 1 by 1 
                                   until idx > 20
                            if gra-da(idx) <= cli-grade and
                               gra-a(idx)  >= cli-grade
                               move gra-perce(idx) to Sum
                               exit perform
                            end-if
                         end-perform
                     end-read
                     if Sum > 0
                        compute cli-fidejussione = 
                                cli-fidejussione * Sum / 100
                     end-if
                 end-if

                 compute sf-lince = 
                         sf-lince         +
                         cli-fidejussione +
                         cli-fido-extra   +
                         cli-pfa
              else                       
                 if tge-blocco-fido < cli-fido
                    compute sf-lince = sf-lince        + 
                                       tge-blocco-fido + 
                                       cli-pfa         +
                                       cli-fido-extra
                 else
                    compute sf-lince = sf-lince +
                                       fido-tmp + 
                                       cli-pfa  +
                                       cli-fido-extra 
                 end-if
              end-if 
           else
              compute sf-lince = sf-lince + 
                                 cli-fido + 
                                 cli-pfa  + 
                                 cli-fidejussione
           end-if.

      *     compute sf-lince = sf-lince         +
      *                        cli-fido         +
      *                        cli-fidejussione +
      *                        cli-pfa|          +
      *                        |cli-fido-extra.

           write sf-rec invalid rewrite sf-rec end-write.

      ***---
       VERIFICA-2.
           accept data-2mesi from century-date.
      
           evaluate mese-2
           when 1
                move 11 to mese-rif
                subtract 1 from anno
           when 2
                move 12 to mese-rif
                subtract 1 from anno
           when other
                subtract 2  from mese-2 giving mese-rif
           end-evaluate.
           move mese-rif to mese-2.

           evaluate mese-rif
           when  1 move 31 to giorno
                   move "Scadenze di Gennaio: " to sitfin-p-scadenze
           when  2 divide   anno      by 4 giving como-anno
                   multiply como-anno by 4 giving como-anno
                   if anno = como-anno
                      move 29 to giorno
                   else
                      move 28 to giorno
                   end-if
                   move "Scadenze di Febbraio: " to sitfin-p-scadenze
           when  3 move 31 to giorno
                   move "Scadenze di Marzo: " to sitfin-p-scadenze
           when  4 move 30 to giorno
                   move "Scadenze di Aprile: " to sitfin-p-scadenze
           when  5 move 31 to giorno
                   move "Scadenze di Maggio: " to sitfin-p-scadenze
           when  6 move 30 to giorno
                   move "Scadenze di Giugno: " to sitfin-p-scadenze
           when  7 move 31 to giorno
                   move "Scadenze di Luglio: " to sitfin-p-scadenze
           when  8 move 31 to giorno
                   move "Scadenze di Agosto: " to sitfin-p-scadenze
           when  9 move 30 to giorno                                             
                   move "Scadenze di Settembre: " to sitfin-p-scadenze
           when 10 move 31 to giorno
                   move "Scadenze di Ottobre: " to sitfin-p-scadenze
           when 11 move 30 to giorno
                   move "Scadenze di Novembre: " to sitfin-p-scadenze
           when 12 move 31 to giorno
                   move "Scadenze di Dicembre: " to sitfin-p-scadenze
           end-evaluate.

           initialize sitfin-p-scaduto-al.
           string "Scaduto al " delimited size
                  giorno        delimited size
                  "/"           delimited size
                  mese-2        delimited size
                  "/"           delimited size
                  anno          delimited size
                  ":"           delimited size
                  into sitfin-p-scaduto-al
           end-string.

           move tge-perce-fido to perce-max-z.

           initialize sitfin-p-perce-max.
           string "(% Max di " delimited size
                  perce-max-z  delimited size
                  ")"          delimited size
                  into sitfin-p-perce-max
           end-string.

           move 0 to num.
           move 0 to den.

           move low-value       to record-pas.
           move data-2mesi      to pas-data-scadenza.
           move 1               to gg of pas-data-scadenza.
           move "C"             to pas-tipo-cfm.
           move link-cli-codice to pas-codice-cfm.
           start PAS key >= pas-codice2
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read pas next at end exit perform end-read
                    if pas-codice-cfm not = link-cli-codice
                       exit perform
                    end-if

                    if pas-data-scadenza > data-2mesi
                       exit perform
                    end-if

                    move pas-progressivo to pat-progressivo
                    read pat no lock key pat-codice
                         invalid continue
                     not invalid
                         if pat-dare-avere-d
                            compute num =
                                    num + pas-importo-dare
                            if pas-situazione = 0 |NON PAGATO
                               compute den =
                                       den + pas-importo-dare
                            end-if
      *****                   else
      *****                      compute num = 
      *****                              num + 
      *****                            ( pas-importo-dare * -1 )
                         end-if
                    end-read

                 end-perform
           end-start.

           move cli-piva to sf-piva.

           read sitfin  no lock
                invalid initialize sf-den sf-num
           end-read.

           add den to sf-den.
           add num to sf-num.

           write sf-rec invalid rewrite sf-rec end-write.

           accept como-data from century-date.
           string "di cui non pagati al " delimited size
                  como-data(7:2)          delimited size
                  "/"                     delimited size
                  como-data(5:2)          delimited size
                  "/"                     delimited size
                  como-data(1:4)          delimited size
                  into sitfin-p-scadenze
           end-string.

      ********---
      ***** VERIFICA-3.
      *****     set  tutto-ok   to true.
      *****
      *****     move low-value  to tmo-rec.
      *****
      *****     set tmo-cliente to true.
      *****     move cli-codice to tmo-cod-clifor.
      *****
      *****     move tge-data-consolid-progmag(1:4)  to como-anno
      *****     compute como-data = 
      *****             function INTEGER-OF-DATE(tge-data-consolid-progmag).
      *****
      ******    controllo l'anno bisestile
      *****     divide como-anno by 4 giving como-anno remainder cont
      *****     if cont = zero
      ******    se è bisestile
      *****        subtract 365 from como-data
      *****     else
      *****        subtract 364 from como-data
      *****     end-if
      *****     compute tmo-data-movim = 
      *****             function DATE-OF-INTEGER(como-data).
      *****
      *****     initialize sitfin-p-fat-per.
      *****     string "Fatturato ("                  delimited size
      *****            tmo-data-movim(7:2)            delimited size
      *****            "/"                            delimited size
      *****            tmo-data-movim(5:2)            delimited size
      *****            "/"                            delimited size
      *****            tmo-data-movim(1:4)            delimited size
      *****            " - "                          delimited size
      *****            tge-data-consolid-progmag(7:2) delimited size
      *****            "/"                            delimited size
      *****            tge-data-consolid-progmag(5:2) delimited size
      *****            "/"                            delimited size
      *****            tge-data-consolid-progmag(1:4) delimited size
      *****            "):"                           delimited size
      *****            into sitfin-p-fat-per
      *****     end-string.
      *****
      *****     start tmovmag key is >= k-data
      *****           invalid set errori to true
      *****     end-start.
      *****
      *****     if tutto-ok
      *****        move 0 to fatturato
      *****
      *****        perform until 1 = 2
      *****
      *****           read tmovmag next at end exit perform end-read
      *****
      *****           if tmo-data-movim > tge-data-consolid-progmag
      *****              exit perform
      *****           end-if
      *****
      *****           if tmo-cliente    and
      *****              tmo-cod-clifor = cli-codice
      *****              move tmo-causale to tca-codice
      *****              read tcaumag no lock invalid continue end-read
      *****
      *****              if tca-cliente and tca-si-stat
      *****                 set  cli-tipo-C     to true
      *****                 move tmo-cod-clifor to cli-codice
      *****                 read clienti no lock
      *****                      invalid continue
      *****                  not invalid
      *****                      perform LOOP-RIGHE-MOVMAG
      *****                 end-read
      *****              end-if
      *****           end-if
      *****
      *****        end-perform
      *****     end-if.
      *****
      *****     move cli-piva to sf-piva.
      *****
      *****     read sitfin  no lock
      *****          invalid initialize sf-fatturato
      *****     end-read.
      *****
      *****     if cli-gg-dilazione > sf-gg-dilazione
      *****        move cli-gg-dilazione to sf-gg-dilazione
      *****     end-if.
      *****
      *****     add fatturato to sf-fatturato.
      *****
      *****     write sf-rec invalid rewrite sf-rec end-write.

      ***---
       LOOP-RIGHE-MOVMAG.
           initialize tab-iva.
           set  tutto-ok     to true.
           move low-value    to rmo-rec.
           move tmo-chiave   to    rmo-chiave.
           start rmovmag key is >= rmo-chiave 
                 invalid set errori to true 
           end-start.
           if tutto-ok
              perform TROVA-ORDINE-NOTA
              perform until 1 = 2
                 read rmovmag  next at end exit perform end-read
                 if rmo-anno   not = tmo-anno or
                    rmo-movim  not = tmo-numero
                    exit perform
                 end-if
                 if rmo-qta = 0
                    compute como-valore = rmo-netto    +
                                          rmo-imp-cons + 
                                          rmo-coubat
                 else
                    compute como-valore = rmo-qta *
                          ( rmo-netto + rmo-imp-cons + rmo-coubat)
                 end-if

                 |Uso i paragrafi per l'addizionale, 
                 |ma valuta il codice iva
                 if trovato-ordine
                    perform TROVA-ADDIZIONALE-ORDINE
                 end-if
                 if trovato-nota
                    perform TROVA-ADDIZIONALE-NOTA
                 end-if

                 if cod-iva not = spaces
                    move "IV"    to tbliv-codice1
                    move cod-iva to tbliv-codice2
                    read tivaese invalid continue end-read

                    perform varying idx from 1 by 1
                              until idx > 3
                       if el-perce-iva(idx) = tbliv-percentuale or
                          el-imponib(idx)   = 0
                          exit perform
                       end-if
                    end-perform

                    move tbliv-percentuale to el-perce-iva(idx)
                    compute el-imponib(idx) = 
                            el-imponib(idx) + como-valore
                 end-if

              end-perform
           end-if.

           move 0 to como-valore.
           perform varying idx from 1 by 1 
                     until idx > 3

              if el-imponib(idx) = 0
                 exit perform
              end-if

              compute como-iva = 
                    ( el-imponib(idx) * 
                      el-perce-iva(idx) ) / 100
              
              add 0,005     to como-iva
              move como-iva to como-iva-2dec
              
              compute como-valore = 
                      como-valore +
                      como-iva-2dec  +
                      el-imponib(idx)
           end-perform.
           
           if tca-imponibile-pos
              add como-valore to fatturato
           else
              subtract como-valore from fatturato
           end-if.

      ***---
       CLOSE-FILES.
           close clienti rmovmag tmovmag tparamge tcaumag PAT PAS
                 tivaese tordini rordini tnotacr rnotacr sitfin 
                 ttipocli grade.

      ***---
       EXIT-PGM.
           goback.

      ***---
       PARAGRAFO-COPY.
           copy "recupero-addizionale.cpy".
