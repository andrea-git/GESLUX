       PROGRAM-ID.     stbrogcmp-auto.
       AUTHOR.         Andrea.
       REMARKS. Partendo da un numero di brogliaccio della tabella
           movimenti trasferiti, genera in automatico un pdf per i 
           clienti con tipologia clienti settato a true (tcl-stampa-brogm-si)

       SPECIAL-NAMES. DECIMAL-POINT is COMMA.

       FILE-CONTROL.    

       SELECT logfile
           ASSIGN       TO  path-log
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE  IS SEQUENTIAL
           FILE STATUS  IS STATUS-logfile.

           copy "mtordini.sl".
           copy "mrordini.sl".
           copy "clienti.sl".
           copy "articoli.sl".
           copy "destini.sl". 
           copy "tvettori.sl".
           copy "agenti.sl".
           copy "tcodpag.sl".
           copy "tivaese.sl".
           copy "progmag.sl".
           copy "tcaumag.sl".
           copy "ttipocli.sl".
           copy "tparamge.sl".
           copy "assorcli.sl".
           copy "lisagente.sl".
           copy "tpiombo.sl".
           copy "tmovmag.sl".
           copy "rmovmag.sl".
           copy "listini.sl".
           copy "tgrupgdo.sl".
           copy "tmarche.sl".
           copy "param.sl".
           copy "tmovtrat.sl".
           copy "tsetinvio.sl".    
           copy "lineseq-mail.sl".

       FILE SECTION.                          

       FD  logfile.
       01 log-riga        PIC  x(900).   

           copy "mtordini.fd".
           copy "mrordini.fd".
           copy "clienti.fd".
           copy "articoli.fd".
           copy "destini.fd". 
           copy "tvettori.fd".
           copy "agenti.fd". 
           copy "tcodpag.fd".
           copy "tivaese.fd".
           copy "progmag.fd".
           copy "tcaumag.fd".
           copy "ttipocli.fd".
           copy "tparamge.fd".
           copy "assorcli.fd".
           copy "lisagente.fd".
           copy "tpiombo.fd".
           copy "tmovmag.fd".
           copy "rmovmag.fd".
           copy "listini.fd".
           copy "tgrupgdo.fd".
           copy "tmarche.fd".
           copy "param.fd".
           copy "tmovtrat.fd".
           copy "tsetinvio.fd".
           copy "lineseq-mail.fd".

       WORKING-STORAGE SECTION.
           copy "imposte.def".
           copy "comune.def".
           copy "spooler.def".
           copy "recupera-prz-listino.def".
           copy "trova-parametro.def".
           copy "fonts.def". 
           copy "link-settaPDF.def".
           copy "selprint.lks". 
           copy "mail.def".

       78  titolo                value "Stampa Brogliaccio".
       77  wfont-status          pic s9(5)   value zero.
       77  como-data             pic 9(8).
       77  como-ora              pic 9(8).
       77  como-imponibile       pic 9(9)v999  value 0.
       77  tot-imponibile        pic 9(9)v99   value 0.
       77  como-ivato            pic 9(9)v999  value 0.
       77  tot-ivato             pic 9(9)v99   value 0.
       77  como-valore           pic 9(9)v99   value 0.
       77  differenza            pic 9(9)v999.
       77  totale                pic 9(6)v999.
       77  tassato               pic 9(6)v999.
       77  tot-qli               pic 9(9)v99.
       77  tot-totali            pic 9(9)v99.
       77  NomeFile              pic x(256).
       77  DestFile              pic x(256).
       77  sav-riga              pic x(200). 
       77  como-riga             pic x(200). 
       77  tot-brog              pic 9(5) value 0.

       77  start-secondi         pic 9(18).
       77  end-secondi           pic 9(18).
       77  tot-secondi           pic 9(18).
       77  hh                    pic 99.
       77  mm                    pic 99.
       77  ss                    pic 99.  
       77  como-fido             pic 9(11)v9(2).


      * STATUS DEI FILES                  
       77  status-lineseq-mail   pic xx.
       77  status-logfile        pic xx.
       77  status-mtordini       pic xx.
       77  status-mrordini       pic xx.
       77  status-clienti        pic xx.
       77  status-articoli       pic xx.
       77  status-destini        pic xx.
       77  status-tvettori       pic xx.
       77  status-agenti         pic xx.
       77  status-tcodpag        pic xx.
       77  status-progmag        pic xx.
       77  status-tcaumag        pic xx.
       77  status-tivaese        pic xx.
       77  status-ttipocli       pic xx.
       77  status-timballi       pic xx.
       77  status-tparamge       pic xx.
       77  status-assorcli       pic xx.
       77  status-lisagente      pic xx.
       77  status-tpiombo        pic xx.
       77  status-tmovmag        pic xx.
       77  status-rmovmag        pic xx.
       77  status-listini        pic xx.
       77  status-tgrupgdo       pic xx.
       77  status-tmarche        pic xx.
       77  status-param          pic xx.
       77  status-tmovtrat       pic xx.   
       77  status-tsetinvio      pic xx.

       77  path-log              pic x(256).
       77  path-lineseq-mail     pic x(256).

      * FLAGS
       77  filler                pic x.
         88 prima-volta          value 1, false 0. 

       01  r-inizio              pic x(25).

       01  testata-1-ord.
      *     05 filler             pic x(10).
           05 t1-cau-descrizione pic x(39).
           05 filler             pic x.
           05 t1-numero-ord      pic z(8).
           05 filler             pic x(1).
           05 t1-data-ord        pic x(8).
           05 filler             pic x(2).
           05 filler             pic x(7)  value "Ord Cli".
           05 filler             pic x(1).
           05 t1-num-ord-cli     pic x(12).
           05 filler             pic x(5)  value space.|"Bolla".
           05 filler             pic x(3).
           05 t1-bolla-ord       pic z(8).
           05 filler             pic x(2).
           05 t1-datbolla-ord    pic x(8).
           05 filler             pic x(2).
           05 filler             pic x(5)  value space.|"Fattura".
           05 filler             pic x(2).
           05 t1-fattura-ord     pic z(8).
           05 filler             pic x(2).
           05 t1-datfattura-ord  pic x(8).
           05 filler             pic x(21) value "  Blocco 500 Kg UTF".
           05 t1-blocco-500      pic x(2).          

       01  testata-2-ord.
           05 filler             pic x(10).
           05 t2-cod-cli-ord     pic z(5).
           05 filler             pic x(5).
           05 t2-ragsoc-ord      pic x(40).
           05 filler             pic x(17).
           05 filler             pic x(6)  value "Agente".
           05 filler             pic x(3).
           05 t2-cod-agente-ord  pic z(5).
           05 filler             pic x(1).
           05 t2-des-agente-ord  pic x(40).

       01  testata-3-ord.
           05 filler             pic x(20).
           05 t3-indirizzo-ord   pic x(40).
           05 filler             pic x(17).
           05 filler             pic x(6)  value "Pagam.".
           05 filler             pic x(3).
           05 t3-cod-pag-ord     pic x(3).
           05 filler             pic x(3).
           05 t3-des-pag-ord     pic x(40).
           05 filler             pic x(2).
           05 t3-grupgdo         pic x(30).

       01  testata-4-ord.
           05 filler             pic x(20).
           05 t4-cap-ord         pic x(5).
           05 filler             pic x(1).
           05 t4-localita-ord    pic x(40).
           05 filler             pic x(1).
           05 t4-prov-ord        pic x(2).
           05 filler             pic x(8).
           05 filler             pic x(9)  value "Corriere:".
           05 t4-cod-vettore-ord pic z(5).
           05 filler             pic x(1).
           05 t4-des-vettore-ord pic x(40).

       01  testata-5-ord.
           05 filler             pic x(10).
           05 filler             pic x(9)  value "Consegna:".
           05 filler             pic x(1).
           05 t5-cod-destino     pic z(5).
           05 filler             pic x(1).
           05 t5-destino-ord     pic x(40).
           05 filler             pic x(1).
           05 t5-indirizzo-ord   pic x(40).
      
       01  testata-6-ord.
           05 filler             pic x(20).
           05 t6-localita-ord    pic x(40).
           05 filler             pic x(7).
           05 t6-prov-ord        pic x(2).   

       01  testata-7-ord.
           05 filler             pic x(2).
           05 filler             pic x(17) value "Dati di consegna:".
           05 filler             pic x(1).
           05 t7-note-1          pic x(19).
           05 filler             pic x(2).
           05 t7-note-data       pic x(8).
           05 filler             pic x(2).
           05 t7-note-2          pic x(30).
           05 filler             pic x(2).
           05 t7-note-3          pic x(30).
           05 filler             pic x(2).
           05 t7-note-4          pic x(30).

       01  testata-fissa-ord.
           05 filler             pic x(2).
           05 filler             pic x(6)  value "Codice".
           05 filler             pic x(5).
           05 filler             pic x(4)  value "Peso".
           05 filler             pic x(1).
           05 filler             pic x(8)  value "Cod. GDO".
           05 filler             pic x(6).
           05 filler             pic x(9)  value "Nr. colli".
           05 filler             pic x(13).
           05 filler             pic x(8)  value "Articolo".
           05 filler             pic x(23).
           05 filler             pic x(5)  value "Pezzi".
           05 filler             pic x(6).
           05 filler             pic x(5)  value "NETTO".
           05 filler             pic x(3).
           05 filler             pic x(3)  value "IVA".
           05 filler             pic x(2).
           05 filler             pic x(9)  value "Pz.Agente".
           05 filler             pic x(2).
           05 filler             pic x(14) value "Ultima vendita".

       01  totalizzatore-finale-ord.
           05 filler             pic x(10).
           05 tot-data-ord       pic x(10).
           05 filler             pic x(3) value " - ".
           05 tot-ora-ord        pic x(5).
           05 filler             pic x(20).
           05 tot-descr-ord      pic x(35).
           05 filler             pic x(1).
           05 tot-qli-ed-ord     pic zzz.zzz.zz9,99.

       01  divisorio.
           05 filler             pic x(155) value all "-".

       01  divisorio-finale.
           05 filler             pic x(155) value all "*". 

       78  max-righe             value 46.
       77  num-righe             pic 99 value 0.
       77  sw-corpo              pic 9  value 0.

       01  riga1-ord.
           05 r-riga-ord         pic 99.
           05 filler             pic x.
           05 r-articolo-ord     pic z(5).
           05 filler             pic x.
           05 r-peso-ord         pic zzz9,999 blank zero.
           05 filler             pic x.
           05 r-cod-art-cli-ord  pic x(11).
           05 filler             pic x.
           05 r-colli-ord        pic z(4).
           05 filler             pic x.
           05 r-imballo-ord      pic x(18).
           05 filler             pic x.
           05 r-des-articolo-ord pic x(30).
           05 filler             pic x(1).
           05 r-qta-ord          pic z(5).
           05 filler             pic x.
           05 r-netto-ord-x      pic x(10).
           05 filler             pic x(1).
           05 r-promo            pic x(1).
           05 filler             pic x(1).
           05 r-cod-iva          pic x(3).
           05 filler             pic x(2) value " (".
           05 r-pz-agente        pic zz.zz9,99 blank zero.
           05 filler             pic x value ")".
           05 filler             pic x(1).                      
           05 r-evadi-dal        pic x(1).       
           05 filler             pic x(1).       
           05 r-data-ord         pic x(8).
           05 filler             pic x.
           05 r-ult-prz-ord      pic zzz.zz9,99 blank zero.
           05 filler             pic x.
           05 r-sottocosto-ord   pic x(2).
           05 filler             pic x.
           05 r-costo-ultimo-ord pic zzz.zz9,99 blank zero.

       77  imballi-ed            pic  zz.zzz.zz9.
       77  r-netto-ord           pic zzz.zz9,99 blank zero.
       77  como-lst-cod-art-cli  pic x(15).

       77  filler                pic 9.
           88  trovato-assorcli  value 1, false 0.

       77  filler                pic 9.
           88 trovato-ultimo-prezzo value 1, false 0.  

       01  riga-totali-ord.
           05 filler             pic x(22) value all"-".
           05 filler             pic x(2).
           05 filler             pic x(15) value "Totale peso Kg:".
           05 filler             pic x(2).
           05 tot-peso-ord       pic zzz.zz9,999 blank zero.
           05 filler             pic x(2).
           05 filler             pic x(11) value "Tassato Kg:".
           05 filler             pic x(2).
           05 tot-tassato-ord    pic zzz.zz9,999 blank zero.
           05 filler             pic x(1).
           05 filler             pic x(10) value "Differenza".
           05 filler             pic x(2).
           05 tot-diff-ord       pic zzz.zz9,99 blank zero. 

       77  como-arrot            pic 9(2)v999.
       77  peso                  pic 9(6)v999.

       77  stbrogcm-a-anno       pic 9(4).
       77  stbrogcm-da-anno      pic 9(4).
       77  stbrogcm-da-num       pic 9(8).
       77  stbrogcm-a-num        pic 9(8).
       77  Courier08             handle of font. 

       77  nargs                 pic 99  comp-1 value 0.  
       77  tot-imposte           pic 9(10)v999.

      ******************************************************************
       PROCEDURE DIVISION.

       DECLARATIVES.      
       copy "mail-decl.cpy".
      ***---
       LOGFILE-ERR SECTION.
           use after error procedure on logfile.
           evaluate status-logfile
           when "00"
           when other continue
           end-evaluate. 

       END DECLARATIVES.
        
      ***---
       MAIN-PRG.
           perform INIT.
           perform APRI-FILES.

           if tutto-ok
              move spaces to tge-chiave
              read tparamge no lock invalid continue end-read
              move tge-anno to tra-anno
              read tmovtrat no lock
             
              perform CICLO-MTORDINI

              if not prima-volta
                 if settaPDF-OK
                    perform CHIUDI-STAMPA
                    perform ASPETTA-PDF
                    if settaPDF-OK
                       rewrite tra-rec
                       move settaPDF-nome-file to como-riga
                       perform SCRIVI-RIGA-LOG
                       move "FILE PDF TROVATO" to como-riga
                       perform SCRIVI-RIGA-LOG
                       perform INVIO-MAIL
                    else
                       move "FILE PDF NON TROVATO" to como-riga
                       perform SCRIVI-RIGA-LOG
                    end-if
                 end-if
              end-if

              perform CHIUDI-FILES
           end-if

           destroy Courier08.
           goback.
 
      ***---
       INIT.
           move 0 to tot-totali tot-qli.
           set prima-volta to true.
           set tutto-ok    to true.
                 
      ***---
       APRI-FILES.                           
           accept como-ora  from time.            
           accept como-data from century-date.
           initialize path-log.
           accept  path-log from environment "BROGM_AUTO_PATH_PDF". 
.          inspect path-log replacing trailing spaces by low-value.
           string  path-log                 delimited low-value
                  "BROGLIACCIO-AUTOMATICO_" delimited size
                  como-data                 delimited size
                  "-"                       delimited size
                  como-ora                  delimited size
                  ".log"                    delimited size
                  into path-log
           end-string.
.          inspect path-log replacing trailing low-value by spaces.
           open output logfile.    

           move "INIZIO ELABORAZIONE" to como-riga.
           perform SCRIVI-RIGA-LOG.
                                        

           move como-ora(1:2) to hh.
           move como-ora(3:2) to mm.
           move como-ora(5:2) to ss.

           compute start-secondi = ( hh * 3600 ) + ( mm * 60   ) + ss.

           move "APERTURA FILES" to como-riga.
           perform SCRIVI-RIGA-LOG.  
 
           open input  mtordini,
                       mrordini,
                       clienti,
                       articoli,
                       tvettori,
                       destini,
                       agenti,
                       tcodpag,
                       progmag,
                       tcaumag,
                       tivaese,
                       ttipocli,
                       tparamge
                       assorcli
                       lisagente
                       tpiombo
                       tmovmag,
                       rmovmag
                       listini
                       tgrupgdo
                       tmarche
                       param.
           open i-o tmovtrat. 

      ***---
       SCRIVI-RIGA-LOG.
           if path-log = spaces exit paragraph end-if.
           inspect como-riga replacing trailing spaces by low-value.
           perform SETTA-INIZIO-RIGA.
           initialize log-riga.
           string r-inizio  delimited size
                  como-riga delimited low-value
             into log-riga
           end-string.
           write log-riga.     

      ***---
       CHIUDI-FILES.
           if path-log not = spaces

              move spaces to como-riga
              perform SCRIVI-RIGA-LOG   

              initialize como-riga
              string "*** BROGLIACCI STAMPATI: " delimited size
                    tot-brog                     delimited size
               into como-riga
              end-string
              perform SCRIVI-RIGA-LOG
              
              move spaces to como-riga
              perform SCRIVI-RIGA-LOG

              accept como-ora from time
              move como-ora(1:2) to hh
              move como-ora(3:2) to mm
              move como-ora(5:2) to ss

              compute end-secondi = ( hh * 3600 ) + ( mm * 60   ) + ss
              compute tot-secondi = end-secondi - start-secondi

              if tot-secondi < 60
                 move tot-secondi to ss
                 initialize como-riga
                 string "ELABORAZIONE TERMINATA IN: ",
                        ss, " SECONDI" delimited size
                        into como-riga
                 end-string
                 perform SCRIVI-RIGA-LOG
              else
                 divide tot-secondi by 60 giving mm remainder ss
                 initialize como-riga
                 string "ELABORAZIONE TERMINATA IN: ",
                         mm, " MINUTI E ", ss, " SECONDI" delimited size
                         into como-riga
                 end-string
                 perform SCRIVI-RIGA-LOG
              end-if
              close logfile

           end-if.

           close mtordini,
                 mrordini,
                 clienti,
                 articoli,
                 tvettori,
                 destini,
                 agenti,
                 tcodpag,
                 progmag,
                 tcaumag,
                 tivaese,
                 ttipocli,
                 tparamge
                 assorcli
                 lisagente
                 tpiombo
                 tmovmag,
                 rmovmag
                 listini
                 tgrupgdo
                 tmarche
                 param
                 tmovtrat.

      ***---
       CICLO-MTORDINI.
           set  tutto-ok to true.
           move tra-anno to mto-anno stbrogcm-a-anno 
                                     stbrogcm-da-anno.
           add 1 to tra-ult-brog-st giving mto-numero stbrogcm-da-num.
           move 9999999           to stbrogcm-a-num.

           start mtordini key >= mto-chiave
                 invalid continue
             not invalid perform SCORRI-MTORDINI
           end-start.

           if tot-totali not = 0
              perform SCRIVI-TOTALIZZATORI-FINALI
           end-if.


      ***---
       SCORRI-MTORDINI.
           perform until 1 = 2
              read mtordini next at end exit perform end-read

              if mto-anno   > stbrogcm-a-anno or
                 mto-numero > stbrogcm-a-num
                 exit perform
              end-if
                 
              perform VALIDA-ORDINE

              if tutto-ok
                 add 1 to tot-brog
                 if prima-volta
                    set prima-volta to false

                    move "CREAZIONE PDF" to como-riga
                    perform SCRIVI-RIGA-LOG

                    perform CREA-PDF          
                    move settaPDF-nome-file to como-riga
                    perform SCRIVI-RIGA-LOG
                    if settaPDF-OK
                       move "FILE PDF CREATO" to como-riga
                       perform SCRIVI-RIGA-LOG

                       accept selprint-stampante 
                              from environment "BROGM_AUTO_STAMPANTE"
                       perform APRI-STAMPA
                       move 1 to spl-riga
                    else              
                       move "FILE PDF NON CREATO" to como-riga
                       perform SCRIVI-RIGA-LOG
                       exit perform
                    end-if
                 end-if
                 perform CICLO-STAMPA
                 move mto-numero to tra-ult-brog-st
              end-if
           end-perform.

      ***---
       VALIDA-ORDINE.
           move 0 to como-imponibile
                     tot-imponibile
                     como-ivato
                     tot-ivato
                     como-valore.
           set tutto-ok to true.
                       
           perform RELAZIONE-MTORDINI-CLIENTI.

           set errori to true.
           if tcl-stampa-brogm-si
              set tutto-ok to true
           end-if.

           if tutto-ok
              perform RELAZIONE-MTORDINI-DESTINI
              perform RELAZIONE-MTORDINI-AGENTI 
              perform RELAZIONE-MTORDINI-TCODPAG
              perform RELAZIONE-MTORDINI-TVETTORI
              perform RELAZIONE-MTORDINI-CAUSALI
           end-if.

      ***---
      * RELAZIONI DELLA TESTATA ORDINI (mtordini)
      ***---
       RELAZIONE-MTORDINI-CLIENTI.
           initialize cli-rec replacing numeric data by zeroes
                                   alphanumeric data by spaces.
           set tutto-ok     to true.
           set cli-tipo-C   to true.
           move mto-cod-cli to cli-codice.
           read clienti key cli-chiave
                invalid continue
             not invalid
                 initialize tcl-rec replacing numeric data by zeroes
                                         alphanumeric data by spaces
                 move cli-tipo to tcl-codice
                 read ttipocli no lock invalid continue end-read
           end-read.

      ***---
       RELAZIONE-MTORDINI-DESTINI.
           initialize des-rec replacing numeric data by zeroes
                                   alphanumeric data by spaces.
           set tutto-ok to true.
           if mto-prg-destino       not = 0
              move mto-cod-cli      to des-codice
              move mto-prg-destino  to des-prog
              read destini invalid continue end-read
           end-if.

      ***---
       RELAZIONE-MTORDINI-AGENTI.
           set tutto-ok to true.
           initialize age-rec replacing numeric data by zeroes
                                   alphanumeric data by spaces.
           if mto-cod-agente not = 0                              
              move mto-cod-agente to age-codice
              read agenti invalid continue end-read
           end-if.

      ***---
       RELAZIONE-MTORDINI-TCODPAG.
           set tutto-ok to true.
           initialize record-tblpa replacing numeric data by zeroes
                                        alphanumeric data by spaces.
           if mto-cod-pagamento not = spaces
              move "PA"              to tblpa-codice1
              move mto-cod-pagamento to tblpa-codice2
              move spaces to tblpa-descrizione1
              move spaces to tblpa-descrizione2
              read tcodpag no lock invalid continue end-read
           end-if.

      ***---
       RELAZIONE-MTORDINI-TVETTORI.
           set tutto-ok to true.
           initialize vet-rec replacing numeric data by zeroes
                                   alphanumeric data by spaces.
           if mto-vettore not = 0                              
              move mto-vettore  to vet-codice
              read tvettori key vet-chiave invalid continue end-read
           end-if. 
             
      ***---
       RELAZIONE-MTORDINI-CAUSALI.
           initialize tca-rec replacing numeric data by zeroes
                                   alphanumeric data by spaces.
           move mto-causale to tca-codice
           read tcaumag invalid continue end-read.

      ***---
      * RELAZIONI DELLE RIGHE DEGLI ORDINI (mrordini)
      ***---
       RELAZIONE-MRORDINI-ARTICOLI.
           move mro-cod-articolo to art-codice.
           
           read articoli key art-chiave invalid initialize art-rec
           end-read.

      ***---
       RELAZIONE-MRORDINI-PROGMAG.
           move spaces           to prg-cod-magazzino.
           move spaces           to prg-tipo-imballo.
           move 0                to prg-peso.
           move mro-cod-articolo to prg-cod-articolo.

           read progmag key prg-chiave
                invalid move 0 to prg-costo-ultimo
           end-read.

      ***---
       RELAZIONE-MRORDINI-IVA.
           move 0           to tbliv-percentuale.
           move "IV"        to tbliv-codice1.
           move mro-cod-iva to tbliv-codice2.
           read tivaese invalid continue end-read.

      ***---
       CICLO-STAMPA.
           move 0 to totale differenza tassato.

           perform STAMPA-INTESTAZIONE

           move mto-anno   to mro-anno.
           move mto-numero to mro-numero.
           move low-values to mro-riga.

           start mrordini key >= mro-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read mrordini next
                      at end
                         perform STAMPA-PIEDE
                         exit perform
                    end-read

                    if mro-anno   not = mto-anno or
                       mro-numero not = mto-numero
                       perform STAMPA-PIEDE
                       exit perform
                    end-if
                    perform RELAZIONE-MRORDINI-TMOVMAG
                    perform RELAZIONE-MRORDINI-IVA

                    perform RELAZIONE-MRORDINI-ARTICOLI
                    perform RELAZIONE-MRORDINI-PROGMAG

                    perform STAMPA-CORPO

                    compute totale = totale       +
                                  ( mro-prg-peso * mro-qta )
                 end-perform
           end-start.


      ***---
       STAMPA-INTESTAZIONE.
           if num-righe > max-righe - 11
              perform SALTO-PAGINA
           end-if.

           initialize t1-cau-descrizione
                      t1-numero-ord
                      t1-data-ord
                      t1-bolla-ord 
                      t1-datbolla-ord
                      t1-fattura-ord
                      t1-datfattura-ord
                      t1-num-ord-cli
                      t2-cod-cli-ord
                      t2-ragsoc-ord
                      t2-cod-agente-ord
                      t2-des-agente-ord
                      t3-indirizzo-ord
                      t3-cod-pag-ord
                      t3-des-pag-ord
                      t3-grupgdo
                      t4-cap-ord
                      t4-localita-ord
                      t4-prov-ord
                      t4-cod-vettore-ord
                      t4-des-vettore-ord
                      t5-cod-destino
                      t5-destino-ord
                      t5-indirizzo-ord
                      t6-localita-ord
                      t6-prov-ord
                      t7-note-1
                      t7-note-data
                      t7-note-2
                      t7-note-3
                      t7-note-4.

      *     Adriano - Stampa descrizione causale
           perform RELAZIONE-MTORDINI-CAUSALI
           move tca-descrizione to t1-cau-descrizione
      *     Adriano - Fine

           move   mto-numero   to t1-numero-ord.
           call "C$JUSTIFY" using t1-numero-ord, "L".
           string mto-data-ordine(7:2) delimited by size
                  "/"                  delimited by size
                  mto-data-ordine(5:2) delimited by size
                  "/"                  delimited by size
                  mto-data-ordine(3:2) delimited by size
                  into t1-data-ord
           end-string.

           if mto-prg-destino = zero
              move cli-superamento-500  to des-superamento-500
           end-if
      *     if cli-superamento-500 = "S"
      *        move "SI" to t1-blocco-500
      *     else
              if des-superamento-500 = "S"
                 move "SI" to t1-blocco-500
              else
                 move "NO" to t1-blocco-500
              end-if
      *     end-if.
           initialize spl-riga-stampa.

           move mto-num-ord-cli to t1-num-ord-cli.
           move testata-1-ord   to spl-riga-stampa.
           perform SCRIVI.

           move mto-cod-cli    to t2-cod-cli-ord.
           call "C$JUSTIFY" using t2-cod-cli-ord, "L".
           move cli-ragsoc-1   to t2-ragsoc-ord.
           move mto-cod-agente to t2-cod-agente-ord.
           move age-ragsoc-1   to t2-des-agente-ord.
           initialize spl-riga-stampa.
           move testata-2-ord to spl-riga-stampa.
           perform SCRIVI.

           move cli-indirizzo     to t3-indirizzo-ord.
           move mto-cod-pagamento to t3-cod-pag-ord. 
           initialize t3-des-pag-ord.
           inspect tblpa-descrizione1 replacing trailing 
                                         spaces by low-value
           string  tblpa-descrizione1 delimited by low-value
                   " "                delimited by size
                   tblpa-descrizione2 delimited by size
                   into t3-des-pag-ord
           end-string.

           move spaces to t3-grupgdo.
           if mto-gdo not = spaces
              move mto-gdo to gdo-codice
              read tgrupgdo no lock 
                   invalid  move "** NON TROVATO **" to gdo-intestazione
              end-read
              move gdo-intestazione to t3-grupgdo
           end-if.
           initialize spl-riga-stampa.
           move testata-3-ord to spl-riga-stampa.
           perform SCRIVI.

           move cli-cap         to t4-cap-ord.
           move cli-localita    to t4-localita-ord.
           move cli-prov        to t4-prov-ord.
           move mto-vettore     to t4-cod-vettore-ord.
           move vet-descrizione to t4-des-vettore-ord.
           initialize spl-riga-stampa.
           move testata-4-ord to spl-riga-stampa.
           perform SCRIVI.           
           
           if des-prog not = 0
              move des-prog      to t5-cod-destino
           end-if
           move des-ragsoc-1  to t5-destino-ord.
           move des-indirizzo to t5-indirizzo-ord.
           initialize spl-riga-stampa.
           move testata-5-ord to spl-riga-stampa.
           perform SCRIVI.

           move des-localita to t6-localita-ord.
           move des-prov     to t6-prov-ord.
           initialize spl-riga-stampa.
           move testata-6-ord to spl-riga-stampa.
           perform SCRIVI.

           if mto-data-note1 not = 0
              string mto-data-note1(7:2) delimited by size
                     "/"                 delimited by size
                     mto-data-note1(5:2) delimited by size
                     "/"                 delimited by size
                     mto-data-note1(3:2) delimited by size
                     into t7-note-data
              end-string
           end-if.
           move mto-note1 to t7-note-1.
           move mto-note2 to t7-note-2.
           move mto-note3 to t7-note-3.
           move mto-note4 to t7-note-4.
           move testata-7-ord to spl-riga-stampa.
           perform SCRIVI.

           move spaces to spl-riga-stampa.
           perform SCRIVI.

           initialize spl-riga-stampa.
           move testata-fissa-ord to spl-riga-stampa.
           perform SCRIVI.

           initialize spl-riga-stampa.
           move divisorio to spl-riga-stampa.
           perform SCRIVI.


      ***---
       SCRIVI-TOTALIZZATORI-FINALI.
           if num-righe > max-righe - 10
              perform SALTO-PAGINA
           end-if.

           initialize tot-data-ord.
           string como-data(7:2) delimited by size
                  "/"            delimited by size
                  como-data(5:2) delimited by size
                  "/"            delimited by size
                  como-data(1:4) delimited by size
                  into tot-data-ord
           end-string.

           initialize tot-ora-ord.
           string como-ora(1:2) delimited by size
                  ":"           delimited by size
                  como-ora(3:2) delimited by size
                  into tot-ora-ord
           end-string.
      *
           move "TOTALE vendite ALTRE q.li:" to tot-descr-ord

           move tot-qli    to tot-qli-ed-ord.

           move spaces to spl-riga-stampa.
           perform SCRIVI.
           move spaces to spl-riga-stampa.
           perform SCRIVI.
           move spaces to spl-riga-stampa.
           perform SCRIVI.
           move spaces to spl-riga-stampa.
           perform SCRIVI.

           initialize spl-riga-stampa.
           move divisorio-finale to spl-riga-stampa.
           perform SCRIVI.

           move spaces to spl-riga-stampa.
           perform SCRIVI.

           initialize spl-riga-stampa.
           move totalizzatore-finale-ord to spl-riga-stampa.
           perform SCRIVI.

           move spaces to spl-riga-stampa.
           perform SCRIVI.

           initialize spl-riga-stampa.
           move divisorio-finale to spl-riga-stampa.
           perform SCRIVI.

      ***---
       STAMPA-CORPO.
           initialize r-riga-ord 
                      r-articolo-ord
                      r-peso-ord
                      r-cod-art-cli-ord
                      r-colli-ord
                      r-imballo-ord
                      r-des-articolo-ord
                      r-qta-ord
                      r-netto-ord
                      r-netto-ord-x
                      r-pz-agente
                      r-promo
                      r-evadi-dal
                      r-cod-iva
                      r-data-ord
                      r-ult-prz-ord
                      r-sottocosto-ord
                      r-costo-ultimo-ord.
           move mro-riga         to r-riga-ord.
           move mro-cod-articolo to r-articolo-ord.
           move r-articolo-ord   to r-articolo-ord.
           move mro-prg-peso     to r-peso-ord.
      *****     if cli-gdo not = spaces
           if tcl-gdo-si or tcl-gdo-opz
              perform TROVA-CODICE-ARTICOLO-ON-LISTINI
           else
              move spaces to como-lst-cod-art-cli
           end-if.
           if como-lst-cod-art-cli = spaces
              perform TROVA-CODICE-ARTICOLO-ON-ASSORCLI
              move asc-cod-articolo-per-cliente to r-cod-art-cli-ord
           else
              move como-lst-cod-art-cli         to r-cod-art-cli-ord
           end-if.
           move mro-num-colli    to r-colli-ord.
           if mro-promo not = 0
              move "P" to r-promo
           end-if.
           if mro-evadi-dal > como-data
              move "E" to r-evadi-dal
           end-if.
LUBEXX     move mro-cod-iva      to r-cod-iva.
           call "C$JUSTIFY"   using r-colli-ord, "R".
BLISTR     if mro-si-blister
BLISTR        move mro-des-imballo to r-imballo-ord
           else
              inspect mro-des-imballo replacing trailing spaces 
                                             by low-value
              move mro-qta-imballi to imballi-ed
              call "C$JUSTIFY"  using imballi-ed, "L"
              initialize r-imballo-ord
              string  mro-des-imballo delimited by low-value
                      " da "          delimited by size
                      imballi-ed      delimited by spaces
                      " x "           delimited by size
                      art-udm-imballo delimited by size
                      into r-imballo-ord
              end-string
           end-if.
           move art-descrizione  to r-des-articolo-ord.
OMAGGI     compute como-valore = mro-qta - mro-qta-omaggi.
OMAGGI     move como-valore      to r-qta-ord.
           call "C$JUSTIFY"   using r-qta-ord, "R".
           if mro-si-omaggio
              move "OMAGGIO"      to r-netto-ord-x
              call "C$JUSTIFY" using r-netto-ord-x, "R"
           else
              move 0 to tot-imposte
              if ttipocli-gdo          
                 compute como-valore = 
                         mro-imponib-merce + 
                         mro-imp-consumo   +
                         mro-imp-cou-cobat +
                         mro-add-piombo

              else
                 move mro-imponib-merce to como-valore

                 compute tot-imposte = 
                         mro-imp-consumo   +
                         mro-imp-cou-cobat +
                         mro-add-piombo
              end-if

              move como-valore to r-netto-ord
              move r-netto-ord to r-netto-ord-x
           end-if.
      *****     call "C$JUSTIFY"   using r-netto-ord, "R".
           if art-prezzo-vendita > mro-prz-unitario
              compute differenza = 
              ( art-prezzo-vendita - mro-prz-unitario )
              compute mro-perce-sconto =
              ( differenza * art-prezzo-vendita ) / 100
      *****        move mro-perce-sconto to r-sconto-ord
           else
              move 0 to mro-perce-sconto
           end-if.

           move art-marca-prodotto to mar-codice.
      *    Adriano - Prezzo agente
           perform RELAZIONE-TORDINI-AGENTI.
           perform RECUPERA-PRZ-LISTINO.
           if prezzo-listino > ( mro-imp-consumo + mro-imp-cou-cobat )
              evaluate true also true
              when mar-si-imposta-consumo also mar-si-cou
                   compute prezzo-listino  =
                           prezzo-listino  -
                           mro-imp-consumo -
                           mro-imp-cou-cobat
              when mar-no-imposta-consumo also mar-no-cou
                   compute prezzo-listino  =
                           prezzo-listino  
              when mar-si-imposta-consumo also mar-no-cou
                   compute prezzo-listino  =
                           prezzo-listino  -
                           mro-imp-consumo 
              when mar-no-imposta-consumo also mar-si-cou
                   compute prezzo-listino  =
                           prezzo-listino  -        
                           mro-imp-cou-cobat
              end-evaluate
           else
              move 0 to prezzo-listino
           end-if.
           move prezzo-listino to r-pz-agente.

      *    Adriano - Fine
           if trovato-ultimo-prezzo
              string tmo-data-movim(7:2) delimited size
                     "/"                 delimited size
                     tmo-data-movim(5:2) delimited size
                     "/"                 delimited size
                     tmo-data-movim(3:2) delimited size
                     into r-data-ord
              end-string
              if ttipocli-gdo
                 compute rmo-netto = rmo-netto  +
                                     rmo-coubat +
                                     rmo-imp-cons
              end-if
              move rmo-netto to r-ult-prz-ord
           end-if.
           call "C$JUSTIFY"   using r-ult-prz-ord, "R".
                              
           if tot-imposte not = 0
              if prg-costo-ultimo > tot-imposte
                 subtract tot-imposte from prg-costo-ultimo
                   giving prg-costo-ultimo
              else
                 move 0 to prg-costo-ultimo
              end-if
           end-if.

           if prg-costo-ultimo > mro-prz-unitario
              move "**"             to r-sottocosto-ord
              move prg-costo-ultimo to r-costo-ultimo-ord
              call "C$JUSTIFY"   using r-costo-ultimo-ord, "R"
           end-if.
           initialize spl-riga-stampa.

           move riga1-ord to spl-riga-stampa.
           move 1 to sw-corpo.
           perform STAMPA-RIGA.
           move 0 to sw-corpo.

OMAGGI     if mro-qta-omaggi not = 0
OMAGGI        move 0                to r-articolo-ord
OMAGGI        move 0                to r-peso-ord
OMAGGI        move spaces           to r-cod-art-cli-ord
OMAGGI        move 0                to r-colli-ord
OMAGGI        move spaces           to r-imballo-ord
OMAGGI        move mro-qta-omaggi   to r-qta-ord
OMAGGI        move "OMAGGIO"        to r-netto-ord-x
OMAGGI        call "C$JUSTIFY"   using r-netto-ord-x, "R"
OMAGGI        move 0                to r-pz-agente
OMAGGI        move 0                to r-ult-prz-ord
OMAGGI        move spaces           to r-sottocosto-ord
OMAGGI        move 0                to r-costo-ultimo-ord
OMAGGI        move riga1-ord        to spl-riga-stampa
OMAGGI        move 1                to sw-corpo 
OMAGGI        move tge-cod-iva-omag to r-cod-iva
OMAGGI        move riga1-ord        to spl-riga-stampa
OMAGGI        perform STAMPA-RIGA
OMAGGI        move 0                to sw-corpo
OMAGGI     end-if.

           compute como-imponibile = como-imponibile + como-valore.

           compute como-ivato      = como-ivato +
                   ( como-valore + 
                 ( ( como-valore * tbliv-percentuale ) / 100) ).

      ***---
       SALTO-PAGINA.
           set spl-salto-pagina to true.
           call "spooler" using spooler-link.
           move 0 to num-righe.
           move 1 to spl-riga.

      ***---
       STAMPA-RIGA.
           move spl-riga-stampa to sav-riga
           if num-righe = max-righe 
              perform SALTO-PAGINA
              if sw-corpo = 1
                 move testata-fissa-ord to spl-riga-stampa
                 perform SCRIVI
                 move divisorio to spl-riga-stampa
                 perform SCRIVI
              end-if
           end-if
           move sav-riga to spl-riga-stampa
           perform SCRIVI.

      ***---
       SCRIVI.
           set  spl-stringa  to true.
           call "spooler" using spooler-link.
           add  0,42 to spl-riga.
           add 1 to num-righe.
             
      ***---
       RELAZIONE-TORDINI-AGENTI.
           set tutto-ok to true.
           initialize age-rec replacing numeric data by zeroes
                                   alphanumeric data by spaces.
           if mto-cod-agente not = 0                              
              move mto-cod-agente to age-codice
              read agenti invalid continue end-read
           end-if.

      ***---
       TROVA-CODICE-ARTICOLO-ON-ASSORCLI.
           set trovato-assorcli to false.
           |1. Cerco per chiave completa
           move spaces           to asc-cod-articolo-per-cliente.
           move cli-gdo          to asc-cod-gruppo-gdo.
           move cli-codice       to asc-cod-cliente.
           move mto-prg-destino  to asc-progressivo-destino.
           move mro-cod-articolo to asc-cod-articolo.
           read assorcli no lock 
                invalid continue 
            not invalid set trovato-assorcli to true
           end-read.

           if not trovato-assorcli
              |2. Elimino il destino
              move 0 to asc-progressivo-destino
              read assorcli no lock
                   invalid  continue
               not invalid  set trovato-assorcli to true
              end-read
           end-if.

           if not trovato-assorcli
              |3. Elimino il cliente cercando cos�
              |   solamente per gruppo-codice
              move 0 to asc-cod-cliente
              read assorcli no  lock
                   invalid  continue
               not invalid  set trovato-assorcli to true
              end-read
           end-if.

           if not trovato-assorcli
              |4. Cerco solo per cliente-articolo solo
              |   se non gdo altrimenti � = al punto 1
      *****        if cli-gdo not = spaces
              if tcl-gdo-si or tcl-gdo-opz
                 move spaces     to asc-cod-gruppo-gdo
                 move cli-codice to asc-cod-cliente
                 read assorcli no lock 
                      invalid continue 
                 end-read
              end-if
           end-if.

           if not trovato-assorcli
              move spaces to asc-cod-articolo-per-cliente
           end-if.

      ***---
       RECUPERA-PRZ-LISTINO.
           move 0 to prezzo-listino.
           if age-listino not = 0
              move age-listino        to lis-codice
              move mro-cod-articolo   to lis-articolo
              read lisagente no lock
                   invalid
                   initialize lis-rec replacing numeric data by zeroes
                                           alphanumeric data by spaces
                   move tge-listino-promo to lis-codice
                   read lisagente no lock
                        invalid perform VALORE-DA-ARTICOLI
                    not invalid
                        perform CONTROLLA-PERIODO-VALIDITA
                        if not listino-valido
                           perform VALORE-DA-ARTICOLI
                        else
                           perform AGGIUNGI-PIOMBO
                        end-if
                   end-read
      *****             move prezzo-listino   to r-pz-agente
               not invalid
                   perform CONTROLLA-PERIODO-VALIDITA
                   if not listino-valido
                      perform VALORE-DA-ARTICOLI
                   else
                      perform AGGIUNGI-PIOMBO
                   end-if
      *****             move prezzo-listino   to r-pz-agente
              end-read
           else
              if age-codice not = 0
                 initialize lis-rec replacing numeric data by zeroes
                                         alphanumeric data by spaces
                 move tge-listino-promo to lis-codice
                 move mro-cod-articolo  to lis-articolo
                 read lisagente no lock
                      invalid perform VALORE-DA-ARTICOLI
                  not invalid 
                      perform CONTROLLA-PERIODO-VALIDITA
                      if not listino-valido
                         perform VALORE-DA-ARTICOLI
                      else
                         perform AGGIUNGI-PIOMBO
                      end-if
                 end-read
      *****           move prezzo-listino   to r-pz-agente
              end-if
           end-if.

      ***---
       CONTROLLA-PERIODO-VALIDITA.
           set listino-valido to false.
           if mto-data-ordine >= lis-data-inizio-old and
              mto-data-ordine <= lis-data-fine-old
              move lis-prezzo-old          to prezzo-listino
              set listino-valido to true
           end-if.

           if mto-data-ordine >= lis-data-inizio-new and
              mto-data-ordine <= lis-data-fine-new
              move lis-prezzo-new          to prezzo-listino
              set listino-valido to true
           end-if.

      ***--- 
       VALORE-DA-ARTICOLI.
           if age-perce-marca(1) not = 0
              move 0 to como-valore
           else
              compute como-valore =
                      art-prezzo-vendita - 
                   (( art-prezzo-vendita * 
                      art-perce-sconto-agente ) / 100)

              if mro-si-omaggio
                 compute como-valore =
                         como-valore - 
                      (( como-valore * age-omaggi ) / 100)
              end-if
           end-if.

           move como-valore to prezzo-listino.

      ***---
       AGGIUNGI-PIOMBO.
           if age-si-add-pb
              move mto-data-ordine to como-data-ordine tpb-data
              move art-marca-prodotto to tpb-marca
              move prezzo-listino     to como-prz-unitario
              move mto-cod-cli     to como-prm-cliente
              move mto-prg-destino to como-prm-destino
              perform ADDIZIONALE-PIOMBO-LISTINO
              add add-piombo to como-prz-unitario
                  giving prezzo-listino
           end-if.

      ***---
       ADDIZIONALE-PIOMBO-LISTINO.
           move 0 to add-piombo.
           start tpiombo key <= tpb-chiave
                 invalid continue
             not invalid
                 read tpiombo previous
                 if tpb-marca = art-marca-prodotto and
                    tpb-data <= como-data-ordine
                                           
                    accept calcolo-piombo
                           from environment "CALCOLO_PIOMBO"
                    if nuovo-calcolo-piombo
                       perform TROVA-PARAMETRO
                    else
                       set prm-add-piombo-perce-si to true
                    end-if
                    if prm-add-piombo-perce-si                 
                       compute risultato-imposte  =
                               como-prz-unitario
                       if art-auto-cobat
                          compute add-piombo-3dec =
                          ( risultato-imposte * tpb-perce-auto ) / 100
                       else
                          compute add-piombo-3dec =
                          ( risultato-imposte * tpb-perce-moto ) / 100
                       end-if
                    else                      
                       compute add-piombo-3dec =
                               art-amperaggio * tpb-euro-ampere
                    end-if   
                 end-if
           end-start.

      ***---
       RELAZIONE-MRORDINI-TMOVMAG.
           |TENTATIVO CON DESTINO VALORIZZATO
           set trovato-ultimo-prezzo to false.
           initialize r-data-ord r-ult-prz-ord.
           set  tmo-cliente     to true.
           move mto-cod-cli     to tmo-cod-clifor.
           move mto-prg-destino to tmo-destino.
           move high-value   to tmo-data-movim.
           start tmovmag key <= k2
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read tmovmag previous at end exit perform end-read
                    if mto-cod-cli not = tmo-cod-clifor
                       exit perform
                    end-if

LUBEXX              if mto-causale = tmo-causale
                       if tmo-anno-fattura  not = 0 and
                          tmo-data-fattura  not = 0
                          perform TROVA-RMOVMAG
                          if trovato-ultimo-prezzo 
                             exit perform 
                          end-if
                       end-if
LUBEXX              end-if                        
                 end-perform
           end-start.

           |TENTATIVO CON DESTINO GENERICO
           if not trovato-ultimo-prezzo
              initialize r-data-ord r-ult-prz-ord

      *        set  tmo-cliente  to true
      *        move mto-cod-cli  to tmo-cod-clifor
      *        move high-value   to tmo-destino
      *        move high-value   to tmo-data-movim
      *        start tmovmag key is <= k2
      *              invalid continue
      *          not invalid
      *              perform until 1 = 2
      *                 read tmovmag previous 
      *                      at end exit perform 
      *                 end-read
      *                 if mto-cod-cli not = tmo-cod-clifor
      *                    exit perform
      *                 end-if
LUBEXX*                 if mto-causale = tmo-causale
      *                    if tmo-anno-fattura  not = 0 and
      *                       tmo-data-fattura  not = 0
      *                       perform TROVA-RMOVMAG
      *                       if trovato-ultimo-prezzo 
      *                          exit perform 
      *                       end-if
      *                    end-if
LUBEXX*                 end-if                        
      *              end-perform
      *        end-start

              set  rmo-cliente        to true
              move mto-cod-cli        to rmo-cod-clifor
              move mto-causale        to rmo-causale
              move mro-cod-articolo   to rmo-articolo
              move high-value         to rmo-data-movim
              start RMOVMAG key <= rmo-chiave-ricerca
                    invalid continue
                not invalid
                    perform until 1 = 2
                       read RMOVMAG previous 
                            at end exit perform 
                       end-read
                       if not rmo-cliente
                          exit perform
                       end-if
                       if mto-cod-cli not = rmo-cod-clifor
                          exit perform
                       end-if
                          
                       if mto-causale not = rmo-causale
                          exit perform
                       end-if
                       if mro-cod-articolo not = rmo-articolo
                          exit perform
                       end-if
      
LUBEXX                 |Escludo i movimenti rettifiche di prezzo
LUBEXX                 if rmo-qta not = 0
                          move rmo-anno  to tmo-anno
                          move rmo-movim to tmo-numero
                          read tmovmag
                             invalid
                                continue
                          end-read
      
                          if tmo-anno-fattura  not = 0 and
                             tmo-data-fattura  not = 0
                             if trovato-ultimo-prezzo 
                                exit perform 
                             end-if
                          end-if       
      
                       end-if
                    end-perform
              end-start

           end-if.

      ***---
       TROVA-RMOVMAG.
           initialize rmo-rec replacing numeric data by zeroes
                                   alphanumeric data by spaces.
           move tmo-anno           to rmo-anno.
           move tmo-numero         to rmo-movim.
           move mro-cod-articolo   to rmo-articolo.
           read rmovmag key is k-articolo
              invalid 
                 set trovato-ultimo-prezzo to false
              not invalid 
LUBEXX          |Escludo i movimenti rettifiche di prezzo
LUBEXX          if rmo-qta not = 0
LUBEXX             set trovato-ultimo-prezzo to true
LUBEXX          end-if
           end-read.

      ***---
       STAMPA-PIEDE.
           if num-righe > max-righe - 5
              perform SALTO-PAGINA
           end-if.

           initialize tot-peso-ord tot-tassato-ord tot-diff-ord.
           move spaces to spl-riga-stampa.
           perform SCRIVI.
           move totale to como-arrot tassato peso.
           if como-arrot not = 0
             if como-arrot > 50
                add 100 to tassato
                move 00 to tassato(5:2)
             else
                move 50 to tassato(5:2)
             end-if
           end-if.
           move 0 to tassato(7:3).
           move tassato        to tot-tassato-ord.
           move peso           to tot-peso-ord.
           subtract tassato  from peso giving differenza.
           move differenza     to tot-diff-ord.
           call "C$JUSTIFY" using tot-peso-ord,    "R".
           call "C$JUSTIFY" using tot-diff-ord,    "R".
           call "C$JUSTIFY" using tot-tassato-ord, "R".
           compute tot-totali = tot-totali + totale
           compute tot-qli    = tot-totali / 100
           initialize spl-riga-stampa.
           move riga-totali-ord to spl-riga-stampa.
           perform SCRIVI.

           move spaces to spl-riga-stampa.
           perform SCRIVI.

           initialize spl-riga-stampa.
           move divisorio-finale to spl-riga-stampa.
           perform SCRIVI.

           move spaces to spl-riga-stampa.
           perform SCRIVI.

           move spaces to spl-riga-stampa.
           perform SCRIVI.
           
      ***---
       TROVA-CODICE-ARTICOLO-ON-LISTINI.
           move spaces to lst-cod-art-cli como-lst-cod-art-cli.
           move low-value to lst-rec.
           move cli-gdo   to lst-gdo.
           move mro-cod-articolo to lst-articolo.
           start listini key >= lst-k-gdo-articolo
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read listini next at end exit perform end-read
                    if lst-gdo      not = cli-gdo or
                       lst-articolo not = mro-cod-articolo
                       exit perform
                    end-if
                    move lst-cod-art-cli to como-lst-cod-art-cli
                 end-perform
           end-start.

      ***---
       CREA-PDF.
           accept DestFile from environment "BROGM_AUTO_PATH_PDF". 
           accept como-data from century-date.
           accept como-ora  from time.                      

           string "BROGLIACCIO-AUTOMATICO_" delimited size
                  como-data(7:2) delimited size
                  "-"            delimited size
                  como-data(5:2) delimited size
                  "-"            delimited size
                  como-data(1:4) delimited size
                  "_"            delimited size
                  como-ora(1:2)  delimited size
                  "-"            delimited size   
                  como-ora(3:2)  delimited size
                  "-"            delimited size   
                  como-ora(5:2)  delimited size
             into NomeFile
           end-string.

           set settaPDF-setta to true.

           move NomeFile  to settaPDF-nome-file.
           move DestFile  to settaPDF-percorso.
           call   "settaPDF2" using settaPDF-linkage.
           cancel "settaPDF2".

      ***---
       ASPETTA-PDF.
           set settaPDF-resetta   to true.
           call   "settaPDF2" using settaPDF-linkage
           cancel "settaPDF2".

      ***---
       APRI-STAMPA.
           if selprint-stampante not = spaces
              move selprint-num-copie to SPL-NUM-COPIE
              move selprint-stampante to SPL-NOME-STAMPANTE

              set spl-horizontal       to true
              set spl-apertura         to true
              move 1                   to spl-margine-sinistro
              move 1                   to spl-margine-destro
              move 1                   to spl-margine-inf
              move "GESLUX - Stampa Brogliaccio automatico" 
                to spl-nome-job
              call "spooler"        using spooler-link
           else
              set spl-sta-annu to true
           end-if.

           if spl-sta-annu    
              move "APERTURA SPOOLER NON RIUSCITA" to como-riga
              perform SCRIVI-RIGA-LOG

              cancel "spooler"
           else                      
              move "APERTURA SPOOLER RIUSCITA" to como-riga
              perform SCRIVI-RIGA-LOG

              initialize WFONT-DATA
              move 08                  to wfont-size
              move "Courier"           to wfont-name
              set wfont-bold           to false
              set wfcharset-dont-care  to true    
              set wfont-italic         to false   
              set wfont-underline      to false   
              set wfont-strikeout      to false   
              set wfont-fixed-pitch    to false   
              move zero                to wfont-char-set
              set wfdevice-win-printer to true
              CALL "W$FONT" USING wfont-get-closest-font, 
                                  Courier08, wfont-data
                                  giving wfont-status
              move Courier08 to spl-hfont
           end-if.

      ***---
       CHIUDI-STAMPA.
           set spl-chiusura to true.
           cancel "spooler".

      ***---
       INVIO-MAIL.
           initialize LinkBody.                  
           accept LinkAddress   from environment "BROGM_AUTO_ADDRESS".
           accept LinkAddressCC from environment "BROGM_AUTO_ADDRESS_CC"

           if LinkAddress not = spaces
      
              move "INVIO MAIL IN CORSO..." to como-riga
              perform SCRIVI-RIGA-LOG

              accept como-data from century-date
              string "INVIO BROGLIACCIO DEL " delimited size
                     como-data(7:2)           delimited size
                     "/"                      delimited size
                     como-data(5:2)           delimited size
                     "/"                      delimited size
                     como-data(1:4)           delimited size
                into LinkSubject
              end-string
              move "In allegato PDF brogliaccio."  to LinkBody
              move settaPDF-nome-file to LinkAttach

              move "stbrogcmp-auto" to NomeProgramma
              move 5 to tentativi-mail
              perform CICLO-SEND-MAIL
           else      
              move "NESSUNA MAIL INVIATA: DESTINATARIO NON VALORIZZATO" 
                to como-riga
              perform SCRIVI-RIGA-LOG
           end-if.

      ***---
       AFTER-SEND-MAIL.
           call "C$DELETE" using path-lineseq-mail, "S".
           if mail-ok
              move "INVIO MAIL RIUSCITO" to como-riga
           else
              move line-riga-mail to como-riga
           end-if.
           perform SCRIVI-RIGA-LOG.

      ***---
       PARAGRAFO-COPY.
           copy "trova-parametro.cpy".
           copy "mail.cpy".
           copy "setta-inizio-riga.cpy".
