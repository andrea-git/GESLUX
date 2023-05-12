       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      stbolle-A4-p.
       AUTHOR.                          Andrea.
       REMARKS. Effettua la stampa di tutti quegli ordini registrati
                come 'BOLLA PRENOTATA' ma non ancora bollettati in 
                quanto sarà questo pgm. stesso ad aggiornare gli ordini
                come bollati (vd. prf. ORDINE-IN-BOLLA).
                Per la ristampa vengono valutati i record già bollettati
                ma non ancora fatturati.
                Di default la stampante ha come margine fisico superiore
                2 righe la prima volta e non vanno considerate proprio.
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           |||STATO ORDINE XXX
           copy "mtordini.sl".
           copy "mrordini.sl".  
           |||XXX
           
           copy "tordini.sl".
           copy "rordini.sl". 
           copy "rordini.sl"
                REPLACING ==rordini==        BY 
                          ==rordini1==,
                          ==STATUS-rordini== BY 
                          ==STATUS-rordini1==.
           copy "clienti.sl".
           copy "destini.sl".
           copy "tvettori.sl".
           copy "articoli.sl".
           copy "lineseq.sl". 
           copy "tivaese.sl".
       SELECT lineseq1
           ASSIGN       TO  wstampa
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE  IS SEQUENTIAL
           FILE STATUS  IS STATUS-lineseq.
       SELECT lineseq2
           ASSIGN       TO  wstampa
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE  IS SEQUENTIAL
           FILE STATUS  IS STATUS-lineseq.
       SELECT lineseq3
           ASSIGN       TO  wstampa
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE  IS SEQUENTIAL
           FILE STATUS  IS STATUS-lineseq.
       SELECT lineseq4
           ASSIGN       TO  wstampa
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE  IS SEQUENTIAL
           FILE STATUS  IS STATUS-lineseq.
       SELECT lineseq5
           ASSIGN       TO  wstampa
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE  IS SEQUENTIAL
           FILE STATUS  IS STATUS-lineseq.
       SELECT line-riep
           ASSIGN       TO  wstampa
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE  IS SEQUENTIAL
           FILE STATUS  IS STATUS-lineseq.
           copy "tcontat.sl".
           copy "tcaumag.sl".
           copy "tnomen.sl".
           copy "listini.sl".
           copy "assorcli.sl".
           copy "reltor.sl".
           copy "ttipocli.sl".
           copy "tparamge.sl".
           copy "prodener.sl".
           copy "tpromo.sl".
           copy "rpromo.sl".
           copy "tscorte.sl".
      *****     copy "evaclides.sl".
           copy "tmarche.sl".
           copy "progmag.sl".
           copy "timposte.sl".
           copy "param.sl".
           copy "tagli.sl".
           
      * tmp corrieri bolla
       SELECT TCB
           ASSIGN       TO path-tcb
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-tcb
           RECORD KEY   IS tcb-chiave
           WITH DUPLICATES .

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.                    
           |||STATO ORDINE XXX
           copy "mtordini.fd".
           copy "mrordini.fd".  
           |||XXX

           copy "tordini.fd".
           copy "rordini.fd".
           copy "rordini.fd"
                REPLACING ==rordini== BY 
                          ==rordini1==,
                          ==STATUS-rordini== BY 
                          ==STATUS-rordini1==.

           copy "clienti.fd".
           copy "destini.fd". 
           copy "tvettori.fd".
           copy "lineseq.fd".   
           copy "tivaese.fd".
      *(( XFD FILE = lineseq1 ))
       FD  lineseq1.
       01 line-riga1        PIC  x(900).
      *(( XFD FILE = lineseq2 ))
       FD  lineseq2.
       01 line-riga2        PIC  x(900).
      *(( XFD FILE = lineseq3 ))
       FD  lineseq3.
       01 line-riga3        PIC  x(900).
      *(( XFD FILE = lineseq4 ))
       FD  lineseq4.
       01 line-riga4        PIC  x(900). 
      *(( XFD FILE = lineseq5 ))
       FD  lineseq5.
       01 line-riga5        PIC  x(900).  
      *(( XFD FILE = lineseq5 ))
       FD  line-riep.
       01 lr-riga           PIC  x(900).

           copy "articoli.fd". 
           copy "tcontat.fd".
           copy "tcaumag.fd".
           copy "tnomen.fd". 
           copy "listini.fd".
           copy "assorcli.fd".
           copy "reltor.fd".
           copy "ttipocli.fd".
           copy "tparamge.fd".
           copy "prodener.fd".
           copy "tpromo.fd". 
           copy "rpromo.fd". 
           copy "tscorte.fd".
      *****     copy "evaclides.fd".
           copy "tmarche.fd".
           copy "progmag.fd".
           copy "timposte.fd".
           copy "param.fd".
           copy "tagli.fd".

       FD  TCB.
       01 tcb-rec.
           05 tcb-chiave.
              10 tcb-vet-codice      pic 9(5).    
              10 tcb-num-bolla       pic 9(8). |0 per testata totali
              10 tcb-data-bolla      pic 9(8). |0 per testata totali
           05 cb-dati.
              10 tcb-tot-bolle       pic 9(3).
              10 tcb-tot-kg          pic 9(12)v999.

       WORKING-STORAGE SECTION.
      * FILE DI COPY           
           copy "link-settaPDF.def".
           copy "acucobol.def".
           copy "acugui.def".
           copy "fonts.def".
           copy "link-geslock.def".
           copy "link-wprogmag.def".
           copy "aggiorna-stato-master.def".
           copy "spooler.def".
           copy "selprint.lks".
           copy "versione-evasione.def".
           copy "trova-parametro.def".     
           
       01  filler             pic 9 value 0.
         88 ControlloCausali  value 1, false 0.
                                            
       01  filler             pic 9 value 0.
         88 gia-fatto-progmag value 1, false 0.
           
       77  eva-anno                pic 9(4) value 0.
       77  eva-da                  pic 9(8) value 0.
       77  eva-a                   pic 9(8) value 0.
                                        
       77  stampante-albaoil       pic x(100).
       77  DestFile                pic x(256).
       77  NomeFile                pic x(256).     
       77  righe-bolle             pic 9(3) value 0.                                  
       77  cont                    pic 9(3) value 0.

       77  path-tcb                pic x(256).

      * FILE STATUS
           |||STATO ORDINE XXX
       77  status-tcb              pic xx.
       77  status-mtordini         pic xx.
       77  status-mrordini         pic xx.
           |||XXX
       77  status-tordini          pic xx.  
       77  status-lineseq          pic xx.
       77  status-destini          pic xx.
       77  status-clienti          pic xx.
       77  status-tvettori         pic xx.
       77  status-articoli         pic xx.
       77  status-tcontat          pic xx.
       77  status-rordini          pic xx.
       77  status-rordini1         pic xx.
       77  status-tcaumag          pic xx.
       77  status-tnomen           pic xx.
       77  status-listini          pic xx.
       77  status-assorcli         pic xx.
       77  status-reltor           pic xx.
       77  status-ttipocli         pic xx.
       77  status-tparamge         pic xx.
       77  status-prodener         pic xx.
       77  status-tpromo           pic xx.
       77  status-rpromo           pic xx.
       77  status-tagli            pic xx.
       77  status-tscorte          pic xx.
      ***** 77  status-evaclides        pic xx.
       77  status-tmarche          pic xx.
       77  status-timposte         pic xx.
       77  status-progmag          pic xx.
       77  status-param            pic xx.    
       77  status-tivaese          pic xx.
       77  wstampa                 pic x(256).
       77  path-globale            pic x(256).
       
       77  rlt-numero-ed    PIC  z(8).
       77  rlt-numero-1     PIC  z(8).
       77  rlt-numero-2     PIC  z(8).
       77  rlt-numero-3     PIC  z(8).
       77  rlt-numero-4     PIC  z(8).
       77  rlt-numero-5     PIC  z(8).
       77  rlt-numero-6     PIC  z(8).
       77  rlt-numero-7     PIC  z(8).
       77  rlt-numero-8     PIC  z(8).
       77  rlt-numero-9     PIC  z(8).
       77  rlt-numero-10    PIC  z(8).
       77  rlt-numero-11    PIC  z(8).
       77  rlt-numero-12    PIC  z(8).
       77  rlt-numero-13    PIC  z(8).
       77  rlt-numero-14    PIC  z(8).
       77  rlt-numero-15    PIC  z(8).

      * FLAGS
           |STATO ORDINE XXX
       77  filler                  pic 9.
           88  trovato-master      value 1, false 0.
                                                    
       77  filler                  pic 9 value 0.
           88  LogoPers                  value 1, false 0.
       77  filler                  pic 9 value 0.
           88  trovato-art               value 1, false 0.
       77  filler                  pic 9 value 0.
           88  GeneraPdf                 value 1, false 0.

       77  tot-master              pic 9(3) value 0.
       01  el-ordine-m             occurs 999 indexed by idx-master.
         05 el-anno-m              pic 9(4).
         05 el-numero-m            pic 9(8).
           |XXX            

       77  CountChar               pic 9(3).
       77  num-bolla-ed            pic z(8).
       77  data-bolla-ed           pic x(10).
       77  dati-bolla              pic x(25).
       77  tot-imp                 pic 9(12)v99.
       77  idx-i                   pic 9 value 0.
       01  tab-iva.
           03 el-iva               occurs 3.
              05 el-cod-iva        pic x(3).
              05 el-imp            pic 9(12)v99.
                                                
       77  como-iva                pic 9(12)v999.
       77  como-iva-2dec           pic 9(12)v99.
       01  filler                  pic 9 value 0.
         88 trovataIva                   value 1, false 0.

       01  GdoInUsoFlag            pic x.
           88 GdoInUso             value "S". 
           88 GdoNonInUso          value " ".
       77  filler                  pic 9.
           88  KBolla              value 1, false 0.
       77  filler                  pic 9.
           88  record-ok           value 1, false 0.
       77  filler                  pic 9.
           88  stampa--segue       value 1, false 0.
       77  filler                  pic 9.
           88  PrimaVolta          value 1, false 0.
       77  filler                  pic 9.
           88  PrimaVolta1         value 1, false 0.
       77  filler                  pic 9.
           88  PrimaVolta2         value 1, false 0.
       77  filler                  pic 9.
           88  PrimaVolta3         value 1, false 0.
       77  filler                  pic 9.
           88  PrimaVolta4         value 1, false 0.
       77  filler                  pic 9.
           88  PrimaVolta5         value 1, false 0.
       77  filler                  pic 9.
           88  esiste-note         value 1, false 0.
       77  FlagTrovato             pic 9.
           88  trovato             value 1, false 0.
       77  filler                  pic 9.
           88  BollaTrovataMaStampaNonPrevista value 1, false 0.
       77  filler                  pic 9.
           88  trovato-assorcli    value 1, false 0.
       77  controllo               pic xx.
           88  tutto-ok            value "OK".
           88  errori              value "ER".
       77  filler                  pic 9.
           88 RecLocked            value 1 false 0.
       77  filler                  pic 9.
           88 CreatoSplit          value 1 false 0.
LUBEXX 77  sw-stampante-default    pic x.
LUBEXX     88 si-stampante-bolle-default value "S", "s".

      * RIGHE PER LA STAMPA
       01  st-riga-titolo-1.                      
         03 cli-riga-titolo-1      pic x(55).     
         03 des-riga-titolo-1      pic x(55).

       01  st-riga-titolo-2.                      
         03 cli-riga-titolo-2      pic x(55).     
         03 des-riga-titolo-2      pic x(55).

       01  st-riga-titolo-3.                      
         03 cli-riga-titolo-3.
           05 cli-st-cap           pic 9(5).
           05 filler               pic x.
           05 cli-st-localita      pic x(49).     
         03 des-riga-titolo-3.
           05 des-st-localita      pic x(55).

       01  st-riga-titolo-4.                      
         03 des-st-cap             pic x(5).      
         03 des-st-prov            pic xx.

       01  st-riga-vettore.                       
         03 st-vettore             pic x(3). 
         03 st-numord              pic z(6).
         03 filler                 pic x(3).
         03 filler                 pic x(14) value "EPAL A RENDERE".

       01  st-num-data.                           
         03 st-num-bolla           pic z(8).      
         03 st-data-bolla          pic x(10).

       01  st-x.
         03 st-elimina             pic x(10) value all "#".
         03 filler                 pic x(2).
         03 filler                 pic x(15)
                                   value "Bolle collegate".
       01 st-testa.
         03 st-cod-cli             pic z(5).
         03 st-num-ord-cli         pic x(12).
         03 st-data-ordine         pic x(8).
         03 st-causale             pic x(18).
         03 st-collegate           pic x(35).

       01  struttura-stampa.
         03 st-dati.                        
           05 st-cod-art           pic z(6).
           05 st-cod-dog           pic x(8).
           05 st-colli             pic z(5).
           05 st-imb               pic x(14).
           05 st-des-art           pic x(30).
           05 st-codart-cli        pic x(10).
           05 st-peso-utf          pic zzz9,999 blank zero.
           05 st-udm               pic xx.                 
           05 st-qta               pic zz.zzz.zzz.         
           05 st-dec-qta           pic x(8). 

OMAGGI 01  st-riga-omaggi.
OMAGGI   03 filler                 pic x(14) value "DI CUI OMAGGIO".
OMAGGI   03 st-qta-oma             pic zz.zzz.zzz.

       01  st-riga-segue.
         03 filler                 pic x(2).
         03 filler                 pic x(104) value all "-".
         03 filler                 pic x.
         03 filler                 pic x(5)  value "Pag. ".
         03 st-num-page            pic z(2).
         03 filler                 pic x(4)  value "  - ".
         03 filler                 pic x(6)  value "Segue ".
         03 filler                 pic x(5)  value "---->".

       01  st-riga-segue-contras.
         03 filler                 pic x(2).
         03 filler                 pic x(104) value all "_".
         03 filler                 pic x.
         03 filler                 pic x(5)  value "Pag. ".
         03 st-num-page-contras    pic z(2).
         03 filler                 pic x(4)  value "  - ".
         03 filler                 pic x(6)  value "Segue ".
         03 filler                 pic x(5)  value "---->".

       01 st-riga-pag.
         03 filler                 pic x(2).                  
         03 filler                 pic x(102) value all "-".
         03 st-pag                 pic x(23).

       01 st-riga-pag-contras.
         03 filler                 pic x(2).                  
         03 filler                 pic x(102) value all "_".
         03 st-pag-contras         pic x(23).

       01  st-riga-piede-1.
         03 filler                 pic x(2).
         03 filler                 pic x(7) value "T.colli".
         03 st-tot-colli           pic z(5).
         03 filler                 pic x(7).
         03 st-note-1              pic x(19).
         03 filler                 pic x.
         03 st-note-data           pic x(10).

       01  st-riga-piede-2.
         03 filler                 pic x(2). 
         03 st-note-2-1            pic x(30).
         03 filler                 pic x(10).
         03 st-note-2-2            pic x(40).
         03 filler                 pic x(10).
         03 st-note-2-3            pic x(40).

       01  st-riga-piede-3.
         03 filler                 pic x(2).
         03 st-note-3-1            pic x(40).
         03 filler                 pic x(15).
         03 st-note-3-2            pic x(40).

       01  st-riga-x.                       
         03 st-mit                 pic x value " ".
         03 st-dest                pic x value " ".
         03 st-vet                 pic x value " ".    
        
       01 st-riga-utf-2710.
         03 st-tot-peso-utf-2710   pic zzz.zzz.zz9,999.

       01 st-riga-utf-3403.
         03 st-tot-peso-utf-3403   pic zzz.zzz.zz9,999.

       01  st-riga-non-utf.           
         03 st-tot-peso-non-utf    pic zzz.zzz.zz9,999.

       01  st-riga-tot.               
         03 st-tot-peso-tot        pic zzz.zzz.zz9,999.
                                   
       01  st-vettore-estesa-1.
         03 st-vet-ragsoc          pic x(72).
                                          
       01  st-vettore-estesa-2.
         03 st-vet-ind             pic x(40).  

       01  st-vettore-estesa-2b.
         03 st-vet-ind2            pic x(40).           

       01  st-ordine-master.
           03 filler               pic x(5).
           03 filler               pic x(16) value "Ordine Cliente: ".
           03 st-om-numordcli      pic x(12).
           03 filler               pic x(6)  value " del: ".
           03 st-om-datacli        pic x(10).
           03 filler               pic x(11) value " - Master: ".
           03 st-om-anno           pic 9(4).
           03 filler               pic x(3)  value " - ".
           03 st-om-numero         pic z(8).

      * VARIABILI
      * DUMMY: NON SERVE, NON SARA' MAI VALORIZZATO. 
       77  user-codi               pic x(15) value spaces. 
       77  limite                  pic 9(3).
       77  imballi-x               pic x(4).
       77  tot-colli               pic 9(5).    
       77  tot-peso-utf-2710       pic 9(9)v999.
       77  tot-peso-utf-3403       pic 9(9)v999.
       77  tot-peso-non-utf        pic 9(9)v999.
       77  tot-peso-tot            pic 9(9)v999.
       77  tot-peso-ed             pic zzz.zzz.zz9,999.
       77  tot-bolle-ed            pic z.zz9.
       77  RowsPerPage             pic 99.
       77  WrittenRows             pic 99.
       77  PagePerBolla            pic 99.
       77  PagePerBollaEdit        pic zz.
       77  scelta                  pic 9.
       77  tor-numero-edit         pic z(8).
       77  ror-qta-z               pic z(8).
       77  como-data               pic 9(8).
       77  como-lst-data           pic 9(8).
       77  como-ora                pic 9(8).
       77  como-bolla              pic 9(8).
       77  contras-note-bolla      pic x(100).
       77  tot-bolla-x             pic x(20).
       77  tot-bolla-z             pic zzz.zzz.zzz.zzz,zz.
       77  tot-bolla-n             pic 9(15)v99.
       01  como-chiave.
         05 como-anno              pic 9(4).
         05 como-numero            pic 9(8).

       01  sav-chiave.
         05 sav-anno               pic 9(4).
         05 sav-numero             pic 9(8).

       77  n-vuote                 pic s99 value 0.
       77  n-vuote-from-divisorio-master pic 99 value 0.
       77  num-bolle-GDO           pic 9(8).
       77  num-bolle-AT            pic 9(8).

      * COSTANTI
       78  titolo                  value "Stampa Bolle".
       78  NumRigheConNote         value 17.
       78  NumRigheSenzaNote       value 20.

       78  max-righe               value 66.
                                                                          
       78  RigheVuoteIntestazione  value 0.
       78  RigheVuotePrimoFoglio   value 0.

LUBEXX***  Usate solo per la prima pagina come
LUBEXX***  scarrellamento fisso iniziale della carta

       77  save-anno              pic 9(4).
       77  save-numero            pic 9(8).
       77  tot-utf                pic 9(5)v999.
       77  utf-singolo            pic 9(5)v999.
       77  idx                    pic 9(5).
       77  idx-coll               pic 9(5).
       77  tot-coll               pic 9(5).
       77  num-righe              pic 9(5).
       77  save-articolo          pic 9(6).
       77  como-lst-cod-art-cli   pic x(15).

      * Memorizzo gli ordini originali da cui sono partiti gli split.
       01  ordine-original  occurs 5000.
         05 anno-orig       pic 9(4).
         05 num-orig        pic 9(8).

       77 idx-stampante     pic 99.
       77 idx-tot-stampanti pic 99 value 0.
       01 el-stampante      pic x(100) occurs 10.
       01 el-albaoil        pic x      occurs 10.

       77  idx-original           pic 9(4) value 0.
       77  num-bolla-da           pic x(8).
       77  num-bolla-a            pic x(8).

       01  old-ror-chiave-ordine-testa.
           05 old-ror-anno-master     PIC  9(4).
           05 old-ror-numero-master   PIC  9(8).

       01                             pic 9.
           88 stampa-corpo            value 1 false zero.
                                       

       77  courier       handle of font.
       77  courier-10    handle of font. 
       77  courier-7     handle of font.
       77  courier-7g    handle of font.
       77  courier-g     handle of font.

       77  messaggio          pic X(150) value spaces.
       77  font-size-dply     pic Z(5).
       77  WFONT-STATUS       pic s9(5) value 0.
       77  BitmapSfondoHandle    pic S9(9) comp-4.

       77  art-no-colli      pic 9(6) occurs 20.
       77  como-art-no-colli pic x(200).


       LINKAGE SECTION.
       copy "link-stbolle.def".

      ******************************************************************
       PROCEDURE DIVISION using stbolle-linkage.

       DECLARATIVES.
       TORDINI-ERR SECTION.
           use after error procedure on tordini.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-tordini 
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File delle testate [TORDINI] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "39"
                display message "File [TORDINI] Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message "[TORDINI] Indexed file corrupt!"
                          title titolo
                           icon 3
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.  

       TCONTAT-ERR SECTION.
           use after error procedure on tcontat.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-tcontat
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File dei contatori [TCONTAT] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "39"
                display message "File [TCONTAT] Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message "[TCONTAT] Indexed file corrupt!"
                          title titolo
                           icon 3
                set errori to true
           when "93"
           when "99" set RecLocked to true
                     set errori to true |< NON PROSEGUO L'ELABORAZIONE
           end-evaluate.  

       LINESEQ-ERR SECTION.
           use after error procedure on lineseq.
           set tutto-ok  to true.
           evaluate status-lineseq
           when "35"
                display message "Impossibile procedere."
                  x"0d0a""File lineseq [LINESEQ] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                display message "File [LINESEQ] Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message "[LINESEQ] Indexed file corrupt!"
                          title titolo
                           icon 3
                set errori to true
           end-evaluate.

      ***---
       RORDINI-ERR SECTION.
           use after error procedure on rordini.
           set tutto-ok  to true.
           evaluate status-rordini
           when "35"
                display message "Impossibile procedere."
                  x"0d0a""File rordini [RORDINI] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                display message "File [RORDINI] Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message "[RORDINI] Indexed file corrupt!"
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
       DESTINI-ERR SECTION.
           use after error procedure on destini.
           set tutto-ok  to true.
           evaluate status-destini
           when "35"
                display message "Impossibile procedere."
                  x"0d0a""File destini [DESTINI] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                display message "File [DESTINI] Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message "[DESTINI] Indexed file corrupt!"
                          title titolo
                           icon 3
                set errori to true
           end-evaluate.

      ***---
       TVETTORI-ERR SECTION.
           use after error procedure on tvettori.
           set tutto-ok  to true.
           evaluate status-tvettori
           when "35"
                display message "Impossibile procedere."
                  x"0d0a""File vettori [TVETTORI] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                display message "File [TVETTORI] Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message "[TVETTORI] Indexed file corrupt!"
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
                  x"0d0a""File articoli [ARTICOLI] inesistente"
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
       TCAUMAG-ERR SECTION.
           use after error procedure on tcaumag.
           set tutto-ok  to true.
           evaluate status-tcaumag
           when "35"
                display message "Impossibile procedere."
                  x"0d0a""File causali magazzino [TCAUMAG] inesistente"
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
       TNOMEN-ERR SECTION.
           use after error procedure on tnomen.
           set tutto-ok  to true.
           evaluate status-tnomen
           when "35"
                display message "Impossibile procedere."
                  x"0d0a""File codici doganali [TNOMEN] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                display message "File [TNOMEN] Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message "[TNOMEN] Indexed file corrupt!"
                          title titolo
                           icon 3
                set errori to true
           end-evaluate.

      ***---
       LISTIINI-ERR SECTION.
           use after error procedure on listini.
           set tutto-ok  to true.
           evaluate status-listini
           when "35"
                display message "Impossibile procedere."
                  x"0d0a""File assortimento [LISTINI] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                display message "File [LISTINI] Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message "[LISTINI] Indexed file corrupt!"
                          title titolo
                           icon 3
                set errori to true
           end-evaluate.

      ***---
       RELTOR-ERR SECTION.
           use after error procedure on reltor.
           set tutto-ok  to true.
           evaluate status-reltor
           when "35"
                display message "Impossibile procedere."
                  x"0d0a""File [RELTOR] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                display message "File [RELTOR] Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message "[RELTOR] Indexed file corrupt!"
                          title titolo
                           icon 3
                set errori to true
           end-evaluate.

      ***---
       TTIPOCLI-ERR SECTION.
           use after error procedure on ttipocli.
           set tutto-ok  to true.
           evaluate status-ttipocli
           when "35"
                display message "Impossibile procedere."
                  x"0d0a""File [TTIPOCLI] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                display message "File [TTIPOCLI] Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message "[TTIPOCLI] Indexed file corrupt!"
                          title titolo
                           icon 3
                set errori to true
           end-evaluate.
                           
      ***---
       TPARAMGE-ERR SECTION.
           use after error procedure on tparamge.
           set tutto-ok  to true.
           evaluate status-tparamge
           when "35"
                display message "Impossibile procedere."
                  x"0d0a""File [TPARAMGE] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                display message "File [TPARAMGE] Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message "[TPARAMGE] Indexed file corrupt!"
                          title titolo
                           icon 3
                set errori to true
           end-evaluate.
                           
      ***---
       MTORDINI-ERR SECTION.
           use after error procedure on mtordini.
           set RecLocked to false.
           evaluate status-mtordini
           when "35"
                display message "File [MTORDINI] not found!"
                           title titolo
                            icon 3
                set errori to true
           when "39"
                display message "File [MTORDINI] Mismatch size!"
                           title titolo
                            icon 3
                set errori to true
           when "98"
                display message "[MTORDINI] Indexed file corrupt!"
                           title titolo
                            icon 3
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.

      ***---
       PRODENER-ERR SECTION.
           use after error procedure on PRODENER.
           set tutto-ok  to true.
           evaluate status-PRODENER
           when "35"
                display message "Impossibile procedere."
                  x"0d0a""File [PRODENER] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                display message "File [PRODENER] Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message "[PRODENER] Indexed file corrupt!"
                          title titolo
                           icon 3
                set errori to true
           end-evaluate.

       END DECLARATIVES.

       MAIN-PRG.
           perform INIT.
           perform OPEN-FILES.
           if tutto-ok
      *****        if stbolle-stampa
      *****           perform CONTROLLA-NUMERAZIONE
      *****        end-if
              if tutto-ok
                 if stbolle-piu-stampe-si
                    perform CONTA-STAMPANTI
                    if tutto-ok 
                       move idx-tot-stampanti to stbolle-num-stampe
                       perform APRI-FILES-STAMPANTI
                    end-if
                 end-if
                 if tutto-ok
                    perform ELABORAZIONE
                    |||STATO ORDINE XXX
                    if stbolle-stampa
                       perform AGGIORNA-MASTER
                    end-if
                    |||XXX                    
                    if stbolle-piu-stampe-si
                       perform CHIUDI-FILES-STAMPANTI
                    end-if
                    if trovato and stbolle-giro = 2
                       perform RIEPILOGO-CORRIERI
                    end-if
                 end-if
              end-if                   
              perform CLOSE-FILES
           end-if.
           perform EXIT-PGM.

      ***---
       INIT.  
           accept contras-note-bolla 
                  from environment "CONTRAS_NOTE_BOLLA".
           if stbolle-stampa
              move 99999999 to stbolle-prima-bolla
           end-if.
           accept stampante-albaoil from environment "STAMPANTE_ALBAOIL"
           inspect stampante-albaoil converting
                               "abcdefghijklmnopqrstuvwxyz" TO
                               "ABCDEFGHIJKLMNOPQRSTUVWXYZ".
           accept como-art-no-colli from environment "ART_NO_COLLI".
           unstring como-art-no-colli delimited by ";"
                    into art-no-colli(1)
                         art-no-colli(2)
                         art-no-colli(3)
                         art-no-colli(4)
                         art-no-colli(5)
                         art-no-colli(6)
                         art-no-colli(7)
                         art-no-colli(8)
                         art-no-colli(9)
                         art-no-colli(10)
                         art-no-colli(11)
                         art-no-colli(12)
                         art-no-colli(13)
                         art-no-colli(14)
                         art-no-colli(15)
                         art-no-colli(16)
                         art-no-colli(17)
                         art-no-colli(18)
                         art-no-colli(19)
                         art-no-colli(20).

           accept versione-evasione from environment "VERSIONE_EVASIONE"
           set no-mail-no-tagli to true.
           
      *****     copy resource ".\STAMPA\sfondo_bolle.bmp".
      *****     if st-graf
      *****        call "W$BITMAP" using WBITMAP-LOAD, "sfondo_bolle.bmp",
      *****                        giving BitmapSfondoHandle
      *****     end-if.

           move stbolle-anno  to save-anno.
           move stb-numero-da to save-numero.

LUBEXX*****     initialize sw-stampante-default.
LUBEXX*****     accept sw-stampante-default 
LUBEXX*****            from environment "STAMPANTE_BOLLE_DEFAULT".
           set BollaTrovataMaStampaNonPrevista to false.
           set stampa--segue to false.
      * POTREBBE VERIFICARSI CHE IL FILE TCONTAT SIA VUOTO!!!!
           if con-num-bolle-gdo = SPACES
              move 0 to con-num-bolle-gdo
           end-if.
           if con-num-bolle-MV = SPACES
              move 0 to con-num-bolle-MV
           end-if.
           if con-num-bolle-AT = SPACES
              move 0 to con-num-bolle-AT
           end-if.
      *-                     
           set     PrimaVolta   to true.
           set     PrimaVolta1  to true.
           set     PrimaVolta2  to true.
           set     PrimaVolta3  to true.
           set     PrimaVolta4  to true.
           set     PrimaVolta5  to true.       
           accept  wstampa      from environment "PATH-ST".
           accept  como-data    from century-date.
           accept  como-ora     from time.
           inspect wstampa      replacing trailing spaces by low-value.
           string wstampa       delimited by low-value
                  "stbolle"     delimited by size
                  "_"           delimited by size
                  como-data     delimited by size
                  "_"           delimited by size
                  como-ora      delimited by size
                  ".txt"        delimited by size
                  into wstampa
           end-string. 
           move wstampa to path-globale.

      ***---
       APRI-FILES-STAMPANTI.
           perform varying idx-stampante from 1 by 1
                     until idx-stampante > idx-tot-stampanti
              accept  wstampa     from environment "PATH-ST"
              accept  como-data   from century-date
              accept  como-ora    from time
              inspect wstampa     replacing trailing spaces by low-value
              string wstampa       delimited by low-value
                     "stbolle"     delimited by size
                     "_"           delimited by size
                     idx-stampante delimited by size
                     "_"           delimited by size
                     como-data     delimited by size
                     "_"           delimited by size
                     como-ora      delimited by size
                     ".txt"        delimited by size
                     into wstampa
              end-string
              evaluate idx-stampante
              when 1 open output lineseq1
                     move wstampa to stbolle-path1
                     move el-stampante(idx-stampante) 
                       to stbolle-stampante1
              when 2 open output lineseq2         
                     move wstampa to stbolle-path2
                     move el-stampante(idx-stampante) 
                       to stbolle-stampante2
              when 3 open output lineseq3         
                     move wstampa to stbolle-path3
                     move el-stampante(idx-stampante) 
                       to stbolle-stampante3
              when 4 open output lineseq4         
                     move wstampa to stbolle-path4
                     move el-stampante(idx-stampante) 
                       to stbolle-stampante4
              when 5 open output lineseq5         
                     move wstampa to stbolle-path5
                     move el-stampante(idx-stampante) 
                       to stbolle-stampante5
              end-evaluate
           end-perform.

      ***---
       CHIUDI-FILES-STAMPANTI.
           perform varying idx-stampante from 1 by 1
                     until idx-stampante > idx-tot-stampanti
              evaluate idx-stampante
              when 1 close lineseq1
              when 2 close lineseq2
              when 3 close lineseq3
              when 4 close lineseq4
              when 5 close lineseq5
              end-evaluate
            continue
           end-perform.
          

      ***---
       OPEN-FILES.                       
           accept como-data  from century-date.
           accept como-ora   from time.

           if stbolle-giro = 2
              initialize path-tcb
              accept  path-tcb  from environment "PATH_ST"
              inspect path-tcb  replacing trailing spaces by low-value
              string  path-tcb  delimited low-value
                      "TCB_"    delimited size
                      como-data delimited size
                      "-"       delimited size
                      como-ora  delimited size
                      ".tmp"    delimited size
                 into path-tcb
              end-string
              inspect path-tcb  replacing trailing low-value by spaces
              open output tcb
              close       tcb
              open i-o    tcb
           end-if.

           set tutto-ok to true.
           if stbolle-stampa
              perform OPEN-IO-TCONTAT
              if tutto-ok
                 perform OPEN-IO-TORDINI
                 if tutto-ok
                    open i-o reltor rordini rordini1 mtordini mrordini
                 end-if
                 if errori
                    close tcontat
                 end-if
              end-if
           else
              open input tordini  tcontat  rordini rordini1 
                         mtordini mrordini 
           end-if.

           if tutto-ok
              open output lineseq
              if tutto-ok
                 open input  clienti  destini  tvettori assorcli param
                             articoli tcaumag  tnomen tmarche timposte
                             listini  reltor   ttipocli |evaclides
                             tparamge prodener tscorte 
                             progmag  tivaese 
                 open i-o tpromo rpromo
              else
                 close tcontat tordini
              end-if
           end-if.

           if errori move spaces to wstampa end-if.

           if tutto-ok

              move space  to tge-codice
              read tparamge invalid continue end-read
           end-if.

      ***---
       OPEN-IO-TORDINI.
           string   "Il file delle testate degli ordini" 
             x"0d0a""risulta in uso su altro terminale."
             x"0d0a""Questo comporta l'impossibilità ad"
             x"0d0a""aggiornare le bolle come fatturate." delimited size
                 into geslock-messaggio
           end-string.

           set tutto-ok  to true.
           open i-o tordini.
           if RecLocked
              move 1 to geslock-v-riprova
              move 0 to geslock-v-ignora
              move 1 to geslock-v-termina
              move   "tordini"    to geslock-nome-file
              call   "geslock" using geslock-linkage
              cancel "geslock"
              evaluate true
              when riprova perform OPEN-IO-TORDINI
              when other   display message box "Operazione interrotta!"
                                   title titolo
                                   icon 2
              end-evaluate
           end-if.

      ***---
       OPEN-IO-TCONTAT.
           set tutto-ok  to true.
           open i-o tcontat allowing readers.
           if RecLocked
              initialize geslock-messaggio
              string "La tabella dei contatori risulta in uso"
              x"0d0a""su altro terminale."
              x"0d0a""Questo comporta il termine dell'esecuzione."
                      delimited size
                 into geslock-messaggio
              end-string
              move 1 to geslock-v-riprova
              move 0 to geslock-v-ignora
              move 1 to geslock-v-termina
              move   "tcontat"    to geslock-nome-file
              call   "geslock" using geslock-linkage
              cancel "geslock"

              if riprova
                 perform OPEN-IO-TCONTAT
              else
                 display message box "Operazione interrotta!"
                         title titolo
                         icon 2
              end-if
           end-if.

      ********---
      ****** GDO: Controllo che il contatore attuale più la somma di tutti i 
      ****** documenti da bollettare non superi i 500000
      ****** AT: Controllo che il contatore parta da un valore superiore
      ****** a 500001 a non sia superiore a 900000
      ****** MV+: Controllo che il contatore parta da un valore superiore
      ****** a 900001
      ***** CONTROLLA-NUMERAZIONE.
      *****     move stbolle-anno to con-anno.
      *****     read tcontat no lock invalid continue end-read.
      *****     if stbolle-gdo or stbolle-at
      *****        set tutto-ok to true
      *****        set trovato to false
      *****        move low-value to tor-rec
      *****
      *****        set tor-bolla-si-prenotata to true
      *****        start tordini key >= k-stbolle
      *****              invalid set errori to true
      *****        end-start
      *****
      *****        if tutto-ok
      *****           perform until 1 = 2
      *****
      *****              read tordini next no lock 
      *****                   at end exit perform 
      *****              end-read
      *****
      *****              set  cli-tipo-C  to true
      *****              move tor-cod-cli to cli-codice
      *****              read clienti no lock invalid continue end-read
      *****              move cli-tipo to tcl-codice
      *****              read ttipocli no lock invalid continue end-read
      *****
      *****              if tor-ordine and tcl-serie-bolle not = 2
      *****              
      *****                 initialize tca-caus-trasporto
      *****                 move tor-causale to tca-codice
      *****                 read tcaumag no lock invalid continue end-read
      *****
      *****                 if tca-si-stampa
      *****                    if tor-bolla-si-prenotata
      *****                       if stbolle-stampa
      *****                          if tor-anno-bolla = 0 and
      *****                             tor-data-bolla = 0 and
      *****                             tor-num-bolla  = 0
      *****                             |Lo split è solo per non GDO
      *****                             evaluate tcl-serie-bolle
      *****                             when 1 add 1 to num-bolle-gdo
      *****                             when 3 add 1 to num-bolle-at
      *****                             end-evaluate
      *****                          else
      *****                             exit perform
      *****                          end-if
      *****                       end-if
      *****                    end-if
      *****                 end-if
      *****              end-if
      *****
      *****           end-perform
      *****
      *****           if stbolle-gdo
      *****              if ( con-num-bolle-GDO + num-bolle-GDO ) > 500000
      *****                 set errori to true
      *****                 display message "Operazione impossibile"
      *****                          x"0d0a""Contatore superiore a 500.000!"
      *****                          x"0d0a""Contattare assistenza!"
      *****                           title titolo
      *****                            icon 2
      *****              end-if
      *****           else
      *****              if ( con-num-bolle-AT + num-bolle-AT ) > 900000
      *****                 set errori to true
      *****                 display message "Operazione impossibile"
      *****                          x"0d0a""Contatore superiore a 900.000!"
      *****                          x"0d0a""Contattare assistenza!"
      *****                           title titolo
      *****                            icon 2
      *****              end-if
      *****           end-if
      *****        end-if
      *****     else
      *****        if con-num-bolle-MV < 900001
      *****           set errori to true
      *****           display message "Operazione impossibile"
      *****                    x"0d0a""Contatore inferiore a 900.001!!!"
      *****                    x"0d0a""Contattare assistenza!"
      *****                     title titolo
      *****                      icon 2
      *****        end-if
      *****     end-if.

      ***---
       CONTA-STAMPANTI.
           set tutto-ok to true.
           set trovato to false.
           move low-value to tor-rec.

           if stbolle-ristampa
              move stbolle-anno  to tor-anno-bolla
              move stb-numero-da to tor-num-bolla
           end-if.

           set KBolla to false.
           if stbolle-singola 
              move stbolle-anno  to tor-anno
              move stb-numero-da to tor-numero
              start tordini key is = tor-chiave
                    invalid set errori to true
              end-start
           else
              if stbolle-k3
                 set tor-bolla-si-prenotata to true
LUBEXX*****      Utilizzo la chiave specifica per questo pgm.
LUBEXX*****      che ha in testa il numero di evasione (pri key)
LUBEXX*****      di modo da non perderne l'ordinamento
LUBEXX           start tordini key >= k-stbolle
LUBEXX*****                 start tordini key is >= k3 |KEY BOLLA
                       invalid set errori to true
                 end-start
              else
                 set KBolla to true
                 start tordini key >= k-bolla
                    invalid set errori to true
                 end-start
              end-if
           end-if.

           if tutto-ok
              perform until 1 = 2

                 read tordini next no lock at end exit perform end-read                    
                 if stbolle-singola
                    if tor-anno   not = save-anno or
                       tor-numero not = save-numero
                       exit perform
                    end-if
                    move tor-num-bolla to stb-numero-da stb-numero-a
                 else
                    if KBolla
                       if tor-anno-bolla not = stbolle-anno or
                          tor-num-bolla  > stb-numero-a
                          exit perform
                       end-if
                    end-if
                 end-if

                 if tor-ordine                                    
                    
                    initialize tca-caus-trasporto
                    move tor-causale to tca-codice
                    read tcaumag no lock invalid continue end-read

                    if tca-si-stampa

                       if stbolle-stampa

                          |Per recuperare la tipologia
                          set  cli-tipo-C  to true
                          move tor-cod-cli to cli-codice
                          read clienti no lock 
                               invalid continue 
                          end-read
                          move cli-tipo to tcl-codice
                          read ttipocli no lock 
                               invalid continue 
                          end-read
                          set record-ok to true
      *****                    if stbolle-GDO and tcl-serie-bolle not = 1
      *****                       set record-ok to false
      *****                    end-if
      *****                    if stbolle-MV  and tcl-serie-bolle not = 2
      *****                       set record-ok to false
      *****                    end-if
      *****                    if stbolle-AT  and tcl-serie-bolle not = 3
      *****                       set record-ok to false
      *****                    end-if

                          if tor-anno-bolla = 0 and
                             tor-data-bolla = 0 and
                             tor-num-bolla  = 0
                             if record-ok
                                if tor-bolla-si-prenotata
LABLAB                             if tor-bloccato
                                      move tor-numero to tor-numero-edit
                                      display message 
                                      "Impossibile procedere."
                               x"0d0a""Sbloccare ordine: " 
                                      tor-anno " - " tor-numero-edit
                               x"0d0a""e procedere di nuovo."
                                              title titolo
                                               icon 2
LABLAB                                set errori to true
LABLAB                             end-if
                                   if tutto-ok
                                      perform AGGIUNGI-STAMPANTE
                                   end-if
                                end-if
                             end-if
                          else
                             exit perform
                          end-if
                       else
                          if tor-anno-bolla not = stbolle-anno and 
                             not stbolle-singola
                             exit perform
                          end-if
                          if tor-anno-bolla = 0 and
                             tor-data-bolla = 0 and
                             tor-num-bolla  = 0
                             exit perform
                          end-if
                          if tor-num-bolla  >=  stb-numero-da and
                             tor-num-bolla  <=  stb-numero-a
      *                       if tor-anno-fattura = 0 and 
      *                          tor-data-fattura = 0 and
      *                          tor-num-prenot   = 0
                                perform AGGIUNGI-STAMPANTE
      *                       end-if
                          end-if
                       end-if
                    end-if
                 end-if

              end-perform
           end-if.

      ***---
       AGGIUNGI-STAMPANTE.
           move tor-cod-cli     to como-prm-cliente.
           move tor-prg-destino to como-prm-destino.
           perform TROVA-PARAMETRO.
           if prm-stampante = spaces
              set errori to true
              display message 
              "CONFIGURARE STAMPANTE PER CLIENTE " tor-cod-cli
                        title titolo
                         icon 2
           else
              move 0 to idx-stampante
              inspect prm-stampante converting
                                  "abcdefghijklmnopqrstuvwxyz" TO
                                  "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
              set trovato to false
              perform until 1 = 2
                 add 1 to idx-stampante
                 if el-stampante(idx-stampante) = spaces
                    exit perform
                 end-if
                 if el-stampante(idx-stampante) = prm-stampante
                    set trovato to true
                    exit perform
                 end-if
              end-perform
              if not trovato
                 add 1 to idx-tot-stampanti
                 move prm-stampante to el-stampante(idx-tot-stampanti)
                 if prm-stampante = stampante-albaoil
                    move 1 to el-albaoil(idx-tot-stampanti)
                 else                                      
                    move 0 to el-albaoil(idx-tot-stampanti)
                 end-if
              end-if
           end-if.

      ***---
       ELABORAZIONE.
           move stbolle-anno to con-anno.
           read tcontat no lock invalid continue end-read.
           set tutto-ok to true.
           set trovato to false.
           move low-value to tor-rec.

           if stbolle-ristampa
              move stbolle-anno  to tor-anno-bolla
              move stb-numero-da to tor-num-bolla
           end-if.

           set KBolla to false.
           if stbolle-singola 
              move stbolle-anno  to tor-anno
              move stb-numero-da to tor-numero
              start tordini key is = tor-chiave
                    invalid set errori to true
              end-start
           else
              if stbolle-k3
                 set tor-bolla-si-prenotata to true
LUBEXX*****      Utilizzo la chiave specifica per questo pgm.
LUBEXX*****      che ha in testa il numero di evasione (pri key)
LUBEXX*****      di modo da non perderne l'ordinamento
LUBEXX           start tordini key >= k-stbolle
LUBEXX*****                 start tordini key is >= k3 |KEY BOLLA
                       invalid set errori to true
                 end-start
              else
                 set KBolla to true
                 start tordini key >= k-bolla
                    invalid set errori to true
                 end-start
              end-if
           end-if.

           if tutto-ok
              perform until 1 = 2

                 read tordini next no lock at end exit perform end-read
                                 
                 if stbolle-singola
                    if tor-anno   not = save-anno or
                       tor-numero not = save-numero
                       exit perform
                    end-if
                    move tor-num-bolla to stb-numero-da stb-numero-a
                 else
                    if KBolla
                       if tor-anno-bolla not = stbolle-anno or
                          tor-num-bolla  > stb-numero-a
                          exit perform
                       end-if
                    end-if
                 end-if

                 if tor-ordine       
                                     
                    perform AGGIUNGI-GG-DATA-CONSEGNA

                    if tor-note1        not = spaces or
                       tor-note2        not = spaces or
                       tor-note3        not = spaces or
                       tor-note4        not = spaces or
                       tor-data-note1   not = 0      or
                       tor-note-bolla-1 not = spaces or
                       tor-note-bolla-2 not = spaces
                       move NumRigheConNote        to RowsPerPage
                       set esiste-note to true
                    else
                       move NumRigheSenzaNote      to RowsPerPage
                       set esiste-note to false
                    end-if      
                    
                    initialize tca-caus-trasporto
                    move tor-causale to tca-codice
                    read tcaumag no lock invalid continue end-read

                    if tca-si-stampa

                       if stbolle-stampa

                          |Per recuperare la tipologia
                          set  cli-tipo-C  to true
                          move tor-cod-cli to cli-codice
                          read clienti no lock 
                               invalid continue 
                          end-read
                          move cli-tipo to tcl-codice
                          read ttipocli no lock 
                               invalid continue 
                          end-read
                          set record-ok to true
      *****                    if stbolle-GDO and tcl-serie-bolle not = 1
      *****                       set record-ok to false
      *****                    end-if
      *****                    if stbolle-MV  and tcl-serie-bolle not = 2
      *****                       set record-ok to false
      *****                    end-if
      *****                    if stbolle-AT  and tcl-serie-bolle not = 3
      *****                       set record-ok to false
      *****                    end-if

                          if tor-anno-bolla = 0 and
                             tor-data-bolla = 0 and
                             tor-num-bolla  = 0
                             if record-ok
                                if tor-bolla-si-prenotata
LABLAB                             if tor-bloccato
LABLAB                                set errori to true
LABLAB                             else
LABLAB                                perform READ-RECORD-LOCK
LABLAB                             end-if
                                   if termina
                                      exit perform
                                   else
                                      if tutto-ok
                                         perform CONTROLLA-500KG-UTF
                                         if CreatoSplit
                                            perform BOLLA-MULTIPLA
                                         else
                                            perform ORDINE-IN-BOLLA   
                                            if stbolle-giro = 1
                                               move " " to st-dest 
                                               move "X" to st-vet 
                                               move " " to st-mit
                                               perform STAMPA-DATI    
                                               move " " to st-vet 
                                               move " " to st-mit
                                               move "X" to st-dest
                                               perform STAMPA-DATI
                                            else                      
                                               move " " to st-vet
                                               move " " to st-dest
                                               move "X" to st-mit
                                               perform STAMPA-DATI
                                            end-if
                                         end-if
                                      end-if
                                   end-if
                                end-if
                             end-if
                          else
                             exit perform
                          end-if
                       else
                          if tor-anno-bolla not = stbolle-anno and 
                             not stbolle-singola
                             exit perform
                          end-if
                          if tor-anno-bolla = 0 and
                             tor-data-bolla = 0 and
                             tor-num-bolla  = 0
                             exit perform
                          end-if
                          if tor-num-bolla  >=  stb-numero-da and
                             tor-num-bolla  <=  stb-numero-a
      *                       if tor-anno-fattura = 0 and 
      *                          tor-data-fattura = 0 and
      *                          tor-num-prenot   = 0 
                                if stbolle-giro = 1
                                   move " " to st-dest      
                                   move "X" to st-vet   
                                   move " " to st-mit
                                   perform STAMPA-DATI
                                   move " " to st-vet
                                   move "X" to st-dest  
                                   move " " to st-mit
                                   perform STAMPA-DATI
                                else
                                   move " " to st-vet
                                   move " " to st-dest  
                                   move "X" to st-mit
                                   perform STAMPA-DATI
                                end-if
      *                       end-if
                          end-if
                       end-if
                    else
                       if tor-bolla-si-prenotata
                          set BollaTrovataMaStampaNonPrevista to true
                       end-if
                    end-if
                 end-if

              end-perform
           end-if.

           if not trovato
              if BollaTrovataMaStampaNonPrevista
                 display message "Stampa bolla non prevista"
                           title titolo
                            icon 2
              else
                 set GeneraPdf to false
                 move 9 to stbolle-giro
                 display message "Nessun documento presente avente"
                                 " il criterio selezionato"
                           title titolo
                            icon 2
              end-if
           else
              |Faccio scrivere solamente sul txt principale
              move 0 to idx-stampante              
                                 
              if stbolle-giro = 1
                 if stbolle-ristampa
                    display message 
                            "Premere OK per avviare la ristampa"
                              title titolo
                 else
                    display message 
                            "Premere OK per avviare la stampa"
                              title titolo
                 end-if
              end-if
           end-if.

      ***---
       READ-RECORD-LOCK.
           set RecLocked to false.
           initialize geslock-linkage.
           move tor-numero to tor-numero-edit.
           string   "Il documento n. " tor-numero-edit
             x"0d0a""risulta in uso su altro terminale."
             x"0d0a""Questo comporta l'impossibilità ad"
             x"0d0a""aggiornarlo come bollato oltre che"
             x"0d0a""ad effettuarne la sua stampa." delimited size
                 into geslock-messaggio
           end-string. 

           set tutto-ok to true.
           read tordini with lock invalid continue end-read.
           perform RIPOSIZIONA-CURSORE-TORDINI.
           if not RecLocked
              perform AGGIUNGI-GG-DATA-CONSEGNA
           else
              move 1 to geslock-v-riprova
              move 1 to geslock-v-ignora
              move 1 to geslock-v-termina
              move "tordini" to geslock-nome-file
              call   "geslock" using geslock-linkage
              cancel "geslock"
              evaluate true
              when riprova perform READ-RECORD-LOCK
              when ignora  set errori to true
                           read tordini no lock
                                invalid continue
                           end-read
                           perform RIPOSIZIONA-CURSORE-TORDINI
              when termina display message box "Operazione interrotta!"
                                   title titolo
                                   icon 2
              end-evaluate
           end-if.

      ***---
       RIPOSIZIONA-CURSORE-TORDINI.
           move tor-chiave to como-chiave.
LUBEXX*****Utilizzo la chiave specifica per questo pgm.
LUBEXX*****che ha in testa il numero di evasione (pri key)
LUBEXX*****di modo da non perderne l'ordinamento
LUBEXX     start tordini key >= k-stbolle
LUBEXX*****           start tordini key is >= k3 invalid continue end-start.
           perform until 1 = 2
              read tordini next no lock at end exit perform end-read
              if tor-chiave = como-chiave exit perform end-if
           end-perform.

      ***---
       CONTROLLA-500KG-UTF.
           set CreatoSplit  to false.
           set  cli-tipo-C  to true.
           move tor-cod-cli to cli-codice.
           read clienti  no lock invalid continue end-read.
           move cli-tipo to tcl-codice.
           read ttipocli no lock invalid continue end-read.
           if cli-utf not = "S"
              move tor-cod-cli     to des-codice
              move tor-prg-destino to des-prog
              read destini no lock invalid continue end-read
              if des-deposito-UTF = "S"
                 exit paragraph
              end-if
           else
              exit paragraph
           end-if.

           move 0 to tot-utf.
           move tor-chiave to ror-chiave   of rordini.
           move low-value  to ror-num-riga of rordini.
           start rordini key >= ror-chiave of rordini.
           perform until 1 = 2
              read rordini next at end exit perform end-read
              if ror-anno       of rordini not = tor-anno or
                 ror-num-ordine of rordini not = tor-numero
                 exit perform
              end-if
              compute tot-utf =
                      tot-utf +
                    ( ror-qta  of rordini * ror-peso-utf of rordini )
              if tot-utf > 500
                 set  CreatoSplit to true
                 move tor-chiave  to sav-chiave
                 perform SPLITTA-ORDINE
                 exit perform
              end-if
           end-perform.

      ***---
       SPLITTA-ORDINE.
           move 1 to tot-utf idx.
           move tor-chiave to ror-chiave   of rordini.
           move low-value  to ror-num-riga of rordini.
           start rordini key >= ror-chiave of rordini.
           perform until 1 = 2
              read rordini next at end exit perform end-read
              if ror-anno       of rordini  not = tor-anno or
                 ror-num-ordine of rordini  not = tor-numero
                 exit perform
              end-if
              if ror-no-omaggio of rordini
                 compute tot-utf = 
                         ror-qta      of rordini *
                         ror-peso-utf of rordini
              end-if
              move ror-rec  of rordini to ror-rec of rordini1
              move ror-num-riga        of rordini 
                to ror-riservato-split of rordini1
              move 9999      to ror-anno       of rordini1
              move 99999999  to ror-num-ordine of rordini1

              if tot-utf > 500
                 perform ror-num-colli of rordini times
                    move idx to ror-num-riga of rordini1
                    compute ror-qta of rordini1 =
                            ror-qta       of rordini / 
                            ror-num-colli of rordini
                    write ror-rec of rordini1 invalid continue end-write
                    add 1 to idx
                 end-perform
              else
                 move idx to ror-num-riga of rordini1
                 write ror-rec of rordini1 invalid continue end-write
                 add 1 to idx
              end-if
              delete rordini record invalid continue end-delete
           end-perform.
           
           move 0 to idx 
           move 1 to idx-coll.
           move 0 to tot-utf.
           move 0 to save-articolo.
           move low-value to ror-rec        of rordini1.
           move 9999      to ror-anno       of rordini1.
           move 99999999  to ror-num-ordine of rordini1.
           start rordini1 key >= ror-chiave of rordini1.
           perform until 1 = 2
              read rordini1 next at end exit perform end-read
              if ror-anno       of rordini1 not = 9999  or
                 ror-num-ordine of rordini1 not = 99999999
                 exit perform
              end-if
              if save-articolo = 0
                 move ror-cod-articolo of rordini1 to save-articolo
              end-if

              if ror-no-omaggio of rordini1
                 compute utf-singolo =
                       ( ror-qta of rordini1 * ror-peso-utf of rordini1)
                 compute tot-utf = 
                         tot-utf + utf-singolo
              end-if

              if idx = 0
                 move tor-chiave to rlt-chiave
                 accept rlt-data-creazione from century-date
                 accept rlt-ora-creazione  from time
                 write rlt-rec invalid continue end-write

                 move tor-numero to como-numero
              end-if
      
              if tot-utf > 500
                 move stbolle-anno to con-anno
                 read tcontat no lock invalid continue end-read
                 add 1 to con-num-ordine
                 rewrite con-rec
                 move con-anno       to tor-anno
                 move con-num-ordine to tor-numero como-numero
                 write tor-rec invalid continue end-write
                 move tor-chiave to rlt-chiave-split(idx-coll)
                 rewrite rlt-rec invalid continue end-rewrite
                 add 1 to idx-coll
                 move idx-coll to tot-coll

                 move 1 to idx
                 move utf-singolo to tot-utf
                 move tor-anno to ror-anno of rordini
                 move ror-riservato-split  of rordini1 
                   to ror-num-riga         of rordini
                 move como-numero to ror-num-ordine of rordini
                 read rordini no lock 
                      invalid
                      move ror-dati of rordini1 to ror-dati of rordini
                  not invalid
                      add  ror-qta  of rordini1 to ror-qta  of rordini
                 end-read
                 compute ror-num-colli   of rordini = 
                         ror-qta         of rordini /
                         ror-qta-imballi of rordini
                 write ror-rec of rordini
                       invalid rewrite ror-rec of rordini
                 end-write
              else
                 move tor-anno    to ror-anno       of rordini
                 move como-numero to ror-num-ordine of rordini
                 move ror-riservato-split of rordini1 to
                      ror-num-riga of rordini
                 read rordini no lock
                      invalid
                      move ror-dati of rordini1 to ror-dati of rordini
                  not invalid
                      add  ror-qta  of rordini1 to ror-qta of rordini
                 end-read
                 compute ror-num-colli   of rordini = 
                         ror-qta         of rordini /
                         ror-qta-imballi of rordini
                 write ror-rec of rordini
                       invalid rewrite ror-rec of rordini
                 end-write
              end-if

           end-perform.

           move low-value to ror-rec        of rordini1.
           move 9999      to ror-anno       of rordini1.
           move 99999999  to ror-num-ordine of rordini1.
           start rordini1 key >= ror-chiave of rordini1.
           perform until 1 = 2
              read rordini1 next at end exit perform end-read
              if ror-anno       of rordini1 not = 9999  or
                 ror-num-ordine of rordini1 not = 99999999
                 exit perform
              end-if
              delete rordini1 record invalid continue end-delete
           end-perform.

      ***---
       STAMPA-DATI.
           move 0 to idx-stampante.
           if stbolle-piu-stampe-si
              
              move tor-cod-cli     to como-prm-cliente
              move tor-prg-destino to como-prm-destino
              perform TROVA-PARAMETRO

              inspect prm-stampante converting
                                  "abcdefghijklmnopqrstuvwxyz" TO
                                  "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

              perform varying idx-stampante from 1 by 1 
                        until idx-stampante > idx-tot-stampanti
                 if prm-stampante = el-stampante(idx-stampante)
                    exit perform
                 end-if
              end-perform
           end-if.
           move 1 to PagePerBolla.
           set trovato to true.
           perform STAMPA-TESTA.
           perform STAMPA-CORPO.
           perform STAMPA-NOTE-TOTALI.

      ***---
       STAMPA-TESTA.
           if stbolle-giro = 2
              initialize tcb-rec replacing numeric data by zeroes
                                      alphanumeric data by spaces
              move tor-vettore to tcb-vet-codice vet-codice
              if vet-codice = 0
                 move "DIRETTO" to vet-descrizione
              else
                 read tvettori no lock
                      invalid move "* NON TROVATO *" to vet-descrizione
                 end-read
              end-if                                     

              read tcb no lock 
                   invalid write tcb-rec
              end-read

              initialize tcb-rec replacing numeric data by zeroes
                                      alphanumeric data by spaces
              move tor-vettore    to tcb-vet-codice
              move tor-num-bolla  to tcb-num-bolla
              move tor-data-bolla to tcb-data-bolla
              read tcb no lock 
                   invalid write tcb-rec
              end-read
           end-if.
                                
           move "@#99" to line-riga.
           perform STAMPA-RIGA.

           move "@#01" to line-riga.
           perform STAMPA-RIGA.
                     
           if prm-layout-pers-si and prm-path-logo not = spaces 
              initialize line-riga
              string "@BMP"        delimited size
                     prm-path-logo delimited size
                into line-riga
              end-string          
              set LogoPers to true
           else
              move "@BMP" to line-riga
              set LogoPers to false
           end-if.                      
           perform STAMPA-RIGA.

           move tor-cod-cli to cli-codice.
           set cli-tipo-C   to true.
           read clienti     no lock invalid continue end-read.
           move cli-tipo to tcl-codice.
           read ttipocli no lock invalid continue end-read.

           move "@B+" to line-riga.
           perform STAMPA-RIGA.

           evaluate idx-stampante
           when 1 
                if not PrimaVolta1        
                   move "@>0020" to line-riga
                   perform STAMPA-RIGA
                   move RigheVuoteIntestazione     to n-vuote
                else
                   move RigheVuotePrimoFoglio      to n-vuote
                   set PrimaVolta1 to false
                end-if
           when 2
                if not PrimaVolta2        
                   move "@>0020" to line-riga
                   perform STAMPA-RIGA
                   move RigheVuoteIntestazione     to n-vuote
                else                                          
                   move RigheVuotePrimoFoglio      to n-vuote
                   set PrimaVolta2 to false
                end-if
           when 3
                if not PrimaVolta3        
                   move "@>0020" to line-riga
                   perform STAMPA-RIGA
                   move RigheVuoteIntestazione     to n-vuote
                else                                          
                   move RigheVuotePrimoFoglio      to n-vuote
                   set PrimaVolta3 to false
                end-if
           when 4
                if not PrimaVolta4        
                   move "@>0020" to line-riga
                   perform STAMPA-RIGA
                   move RigheVuoteIntestazione     to n-vuote
                else                                          
                   move RigheVuotePrimoFoglio      to n-vuote
                   set PrimaVolta4 to false
                end-if
           when 5
                if not PrimaVolta5        
                   move "@>0020" to line-riga
                   perform STAMPA-RIGA
                   move RigheVuoteIntestazione     to n-vuote
                else                                          
                   move RigheVuotePrimoFoglio      to n-vuote
                   set PrimaVolta5 to false
                end-if
           end-evaluate  

      *****     if not PrimaVolta
      *****        move RigheVuoteIntestazione     to n-vuote
      *****     else
LUBEXX*****        evaluate tcl-serie-bolle
      *****        when 1 |stbolle-GDO
LUBEXX*****             subtract 1 
      *****                 from RigheVuoteIntestazioneCompuprint 
LUBEXX*****               giving n-vuote
      *****        when 2 |stbolle-MV
      *****        when 3 |stbolle-AT
      *****             subtract 1 
      *****                 from RigheVuoteIntestazioneEpsonDFX
LUBEXX*****               giving n-vuote
LUBEXX*****        end-evaluate
      *****
      *****        set PrimaVolta to false
      *****     end-if    

           if el-albaoil(idx-stampante) = 1
              subtract 14 from n-vuote
              perform RIGHE-VUOTE
           
              move "ORA" to line-riga
              perform STAMPA-RIGA
              move "ALBRI SRL" to line-riga
              perform STAMPA-RIGA
              move "VIA DONIZETTTI 1/2" to line-riga
              perform STAMPA-RIGA
              move "10022 CARMAGNOLA" to line-riga
              perform STAMPA-RIGA
              move "TEL E FAX 011/9715432" to line-riga
              perform STAMPA-RIGA
           
              move 9 to n-vuote
              perform RIGHE-VUOTE
           else
              perform RIGHE-VUOTE
           end-if.

           initialize des-rec replacing numeric data by zeroes
                                   alphanumeric data by spaces.

           move tor-cod-cli     to des-codice.
           move tor-prg-destino to des-prog.
           read destini no lock invalid continue end-read.

           move cli-ragsoc-1   to cli-riga-titolo-1.  
           if des-ragsoc-1 not = space
              move des-ragsoc-1   to des-riga-titolo-1
           else
              move cli-ragsoc-1   to des-riga-titolo-1
           end-if              
           initialize line-riga.
           move st-riga-titolo-1 to line-riga.
           perform STAMPA-RIGA.    

           move cli-indirizzo  to cli-riga-titolo-2.
           if des-indirizzo not = space
              move des-indirizzo  to des-riga-titolo-2
           else
              move cli-indirizzo  to des-riga-titolo-2
           end-if
           initialize line-riga.
           move st-riga-titolo-2 to line-riga.
           perform STAMPA-RIGA.

           move cli-cap      to cli-st-cap.
           move cli-localita to cli-st-localita.
           if des-localita not = space
              move des-localita to des-st-localita
           else
              move cli-localita to des-st-localita
           end-if
           initialize line-riga.
           move st-riga-titolo-3 to line-riga.
           perform STAMPA-RIGA.
                  
           if des-cap not = spaces
              move des-cap to des-st-cap
           else
              move cli-cap to des-st-cap
           end-if.
           if des-prov not = space
              move des-prov     to des-st-prov
           else
              move cli-prov     to des-st-prov
           end-if.
           move "@#02" to line-riga.
           perform STAMPA-RIGA.

           initialize line-riga.
           move st-riga-titolo-4 to line-riga.
           perform STAMPA-RIGA.

           move tor-vettore      to vet-codice.
           read tvettori 
                invalid move spaces          to st-vettore
                                                st-vettore-estesa-1
                                                st-vettore-estesa-2
            not invalid 
                initialize st-vet-ragsoc
                inspect vet-descrizione replacing
                                         trailing spaces by low-value
                string vet-descrizione  delimited low-value
                       " - "            delimited size
                       vet-piva         delimited size
                       " - "            delimited size
                       vet-n-albo       delimited size
                       into st-vet-ragsoc
                end-string
                move vet-sigla       to st-vettore
                move vet-indirizzo   to st-vet-ind
           end-read.

           move tor-numero     to st-numord.
           call "C$JUSTIFY" using st-numord, "R".
                               
           initialize line-riga.
           perform STAMPA-RIGA.
           move "@+3" to line-riga.
           perform STAMPA-RIGA.
                                 
           move "@#03" to line-riga.
           perform STAMPA-RIGA.

           move st-riga-vettore          to line-riga.
           perform STAMPA-RIGA. 

           move "@-3" to line-riga.
           perform STAMPA-RIGA.
                                 
           move "@#77" to line-riga.
           perform STAMPA-RIGA.
                  
           if st-vet = "X"
              move "COPIA VETTORE *** EPAL A RENDERE ***" 
                to line-riga
           end-if.
           if st-mit = "X"
              move "COPIA MITTENTE *** EPAL A RENDERE ***" 
                to line-riga
           end-if.
           if st-dest = "X"
              move "COPIA DESTINATARIO *** EPAL A RENDERE ***" 
                to line-riga
           end-if.
           perform STAMPA-RIGA.

           move 1 to n-vuote.
           perform RIGHE-VUOTE.

      *****     if stbolle-stampa
      *****        if not stampa--segue
      *****           add 1 to con-num-bolle
      *****           move con-num-bolle to tor-num-bolla
      *****        end-if
      *****     end-if.
           set stampa--segue to false.

           move tor-num-bolla  to st-num-bolla.
           call "C$JUSTIFY" using st-num-bolla, "R".

           if stbolle-stampa
              move stbolle-data to tor-data-bolla
              accept tor-data-bolla-effettiva from century-date
           end-if.
           
           move all "/"             to st-data-bolla.
           move tor-data-bolla(1:4) to st-data-bolla(7:4).
           move tor-data-bolla(5:2) to st-data-bolla(4:2).
           move tor-data-bolla(7:2) to st-data-bolla(1:2).
           
           move "@<0020" to line-riga.
           perform STAMPA-RIGA.
                          
           move "@#04" to line-riga.
           perform STAMPA-RIGA.      

           move "@>0001" to line-riga.
           perform STAMPA-RIGA.

           initialize line-riga.
           move st-num-data to line-riga.
           perform STAMPA-RIGA.
           
           move tor-cod-cli     to st-cod-cli .
           call "C$JUSTIFY"  using st-cod-cli, "R".

           move tor-num-ord-cli to st-num-ord-cli.
           call "C$JUSTIFY"  using st-num-ord-cli, "R".

           if tor-data-ordine not = 0
              move all "/"              to st-data-ordine
              move tor-data-ordine(3:2) to st-data-ordine(7:2)
              move tor-data-ordine(5:2) to st-data-ordine(4:2)
              move tor-data-ordine(7:2) to st-data-ordine(1:2)
           end-if.

           move tca-caus-trasporto  to st-causale.

           initialize st-collegate.
           if stbolle-stampa
              if CreatoSplit
                 perform NUMERI-BOLLA-COLLEGATI
              else
                 move spaces to line-riga
                 perform STAMPA-RIGA
      *           move "N.REG.PILE IT09060P00001136"
      *               to st-collegate
                 initialize st-collegate
                 string "N.REG.PILE " delimited by size
                        tge-reg-PILE  delimited by size
                        into st-collegate
              end-if
           else
              move tor-ordine-testa to rlt-chiave
              read reltor no lock 
                   invalid
                   move spaces to line-riga
                   perform STAMPA-RIGA
      *             move "N.REG.PILE IT09060P00001136"
      *               to st-collegate
                   initialize st-collegate
                   string "N.REG.PILE " delimited by size
                          tge-reg-PILE  delimited by size
                          into st-collegate
                   end-string
               not invalid
                   perform varying tot-coll from 1 by 1 
                             until tot-coll > 15 
                      if rlt-numero-s(tot-coll) = 0
                         exit perform
                      end-if
                   end-perform
                   perform NUMERI-BOLLA-COLLEGATI
              end-read
           end-if.             
                               
           move "@#06" to line-riga.
           perform STAMPA-RIGA.

           move "@<0005" to line-riga.
           perform STAMPA-RIGA.

           initialize line-riga.
           move st-testa to line-riga.
           perform STAMPA-RIGA.

      ***---
       NUMERI-BOLLA-COLLEGATI.    
           move "@#16" to line-riga.
           perform STAMPA-RIGA.

           move st-x    to line-riga
           perform STAMPA-RIGA.

           add rlt-num-bolla to tot-coll 
                    giving como-bolla.
           subtract 1 from como-bolla.

           move rlt-num-bolla to num-bolla-da.
           move como-bolla    to num-bolla-a.
           inspect num-bolla-da replacing leading x"30" by x"20".
           inspect num-bolla-a  replacing leading x"30" by x"20".
           call "C$JUSTIFY" using num-bolla-da, "L".
           call "C$JUSTIFY" using num-bolla-a,  "L".
           inspect num-bolla-da replacing leading x"20" by low-value.
           inspect num-bolla-a  replacing leading x"20" by low-value.
           string "Da n. "     delimited size
                  num-bolla-da delimited low-value
                  "a n. "      delimited size
                  num-bolla-a  delimited low-value
                  into st-collegate
           end-string.
           
      ***---
       STAMPA-CORPO.               
           move 1 to n-vuote.
           perform RIGHE-VUOTE.    
   
           if LogoPers         
              move "@<0010" to line-riga
              perform STAMPA-RIGA
           end-if.         

           move tor-anno   to ror-anno of rordini.
           move tor-numero to ror-num-ordine of rordini.
           move low-value  to ror-num-riga of rordini
                              ror-chiave-ordine OF rordini

           start rordini key is >= ror-k-stbolle of rordini
                 invalid set errori to true
           end-start.
              
           move 0 to tot-bolla-n.  
           initialize tab-iva replacing numeric data by zeroes
                                   alphanumeric data by spaces.
       
           perform until 1 = 2

              read rordini next  at end exit perform end-read
              if ror-anno       of rordini not = tor-anno    or 
                 ror-num-ordine of rordini not = tor-numero
                 exit perform
              end-if

              compute tot-imp = ror-qta of rordini *
                              ( ror-imp-consumo   of rordini +
                                ror-imp-cou-cobat of rordini +
                                ror-imponib-merce of rordini +
                                ror-add-piombo    of rordini )
                    
              set trovataIva to false
              perform varying idx-i from 1 by 1 
                        until idx-i > 3
                 if ror-cod-iva of rordini = el-cod-iva(idx-i)
                    add tot-imp    to el-imp(idx-i)
                    set trovataIva to true
                    exit perform
                 end-if
              end-perform

              if not trovataIva      
                 perform varying idx-i from 1 by 1 
                           until idx-i > 3
                    if el-cod-iva(idx-i) = spaces
                       move ror-cod-iva of rordini to el-cod-iva(idx-i)
                       add tot-imp      to el-imp(idx-i)
                       exit perform
                    end-if
                 end-perform
              end-if

           end-perform.
                                          
           move 0 to como-iva tot-bolla-n.
           perform varying idx-i from 1 by 1 
                     until idx-i > 3
              if el-cod-iva(idx-i) = spaces
                 exit perform
              end-if
              move "IV"              to tbliv-codice1
              move el-cod-iva(idx-i) to tbliv-codice2
              read tivaese no lock
              move 0 to como-iva-2dec
              if tbliv-percentuale > 0
                 compute como-iva = 
                         el-imp(idx-i) * tbliv-percentuale / 100
                 add 0,005          to como-iva
                 move como-iva      to como-iva-2dec
              end-if
              compute tot-bolla-n = tot-bolla-n   +
                                    como-iva-2dec + el-imp(idx-i)
           end-perform.

           move tot-bolla-n to tot-bolla-z.
           move tot-bolla-z to tot-bolla-x.
           call "C$JUSTIFY" using tot-bolla-x, "L".
           inspect tot-bolla-x replacing trailing spaces by low-value.

           move tor-anno   to ror-anno of rordini.
           move tor-numero to ror-num-ordine of rordini.
           move low-value  to ror-num-riga of rordini
                              ror-chiave-ordine OF rordini

           start rordini key is >= ror-k-stbolle of rordini
                 invalid set errori to true
           end-start.

           initialize old-ror-chiave-ordine-testa
           move 0 to tot-colli
                     tot-peso-utf-2710
                     tot-peso-utf-3403
                     tot-peso-non-utf
                     tot-peso-tot
                     WrittenRows.
                                  
           perform until 1 = 2

              read rordini next  at end exit perform end-read
              if ror-anno       of rordini not = tor-anno    or 
                 ror-num-ordine of rordini not = tor-numero
                 exit perform
              end-if

              |STATO ORDINE XXX
              if stbolle-stampa
                 if ror-anno-master   of rordini not = 0 and
                    ror-numero-master of rordini not = 0
                    set trovato-master to false
                    set idx-master to 1
                    search el-ordine-m
                    when el-ordine-m(idx-master) = 
                         ror-chiave-ordine-testa of rordini
                         set trovato-master to true
                    end-search
                    if not trovato-master
                       add 1 to tot-master
                       move ror-chiave-ordine-testa of rordini
                         to el-ordine-m(tot-master)
                    end-if
                 end-if
              end-if
              |XXX

              set stampa-corpo  to true

              perform SALTO-PAGINA

              set stampa-corpo  to false

              if ror-chiave-ordine-testa of rordini not = 
             old-ror-chiave-ordine-testa
                 if ror-numero-master of rordini not = 0
                    perform DIVISORIO-MASTER   
                 end-if
      *           move     ror-chiave-ordine-testa of rordini
      *             to old-ror-chiave-ordine-testa
              end-if

              initialize st-dati replacing numeric data by zeroes 
                                      alphanumeric data by spaces
              move ror-cod-articolo  of rordini to st-cod-art art-codice
              read articoli no lock invalid continue end-read       
              move spaces to pen-sdoppia-riga
              if art-cod-doganale = 0
                 move "*" to st-cod-dog
              else
                 move art-cod-doganale to nom-codice
                 read tnomen no lock
                      invalid move all "*" to st-cod-dog
                  not invalid
                      if nom-si-olio
                         if art-cod-prodener not = spaces
                            move art-cod-prodener to pen-codice
                            read prodener  no lock 
                                 invalid move art-cod-doganale(1:4) 
                                           to st-cod-dog                
                             not invalid move pen-nc to st-cod-dog
                            end-read
                         else
                            move art-cod-doganale(1:4) to st-cod-dog
                         end-if
                      else                 
                         if nom-si-cobat
                            accept st-cod-dog 
                            from environment "CODICE_SICUREZZA_BATTERIE"
                         else
                            move all "*" to st-cod-dog
                         end-if
                      end-if
                 end-read
              end-if
           
              set trovato-art to false
              move 0 to idx
              perform 20 times
                 add 1 to idx
                 if art-codice = art-no-colli(idx)
                    set trovato-art to true
                    exit perform
                 end-if
              end-perform

              if trovato-art
                 add  0 to tot-colli
                 move 0 to st-colli
              else
                 add  ror-num-colli of rordini to tot-colli
                 move ror-num-colli of rordini to st-colli
              end-if

              perform DESCRIZIONE-IMBALLO
              move art-descrizione      to st-des-art
              if ror-si-omaggio of rordini
                 move "OMAGGIOWWX"  to st-codart-cli
              else
              
                 move tor-cod-cli     to como-prm-cliente
                 move tor-prg-destino to como-prm-destino
                 perform TROVA-PARAMETRO

                 if prm-cod-art-forn-si
                    move art-cod-art-frn to st-codart-cli
                 else
                    move spaces to st-codart-cli
      *****              if cli-gdo not = spaces
                    if tcl-gdo-si or tcl-gdo-opz
                       perform TROVA-CODICE-ARTICOLO-ON-LISTINI
                       move como-lst-cod-art-cli to st-codart-cli
                    end-if
                    if st-codart-cli = spaces
                       perform TROVA-CODICE-ARTICOLO-ON-ASSORCLI
                       move asc-cod-articolo-per-cliente 
                         to st-codart-cli
                    end-if
                 end-if
              end-if
              move ror-peso-utf of rordini to st-peso-utf
      *****        add  ror-peso-utf         to tot-peso-utf
      *****        add  ror-peso-non-utf     to tot-peso-non-utf
      *****        add  ror-peso-utf to ror-peso-non-utf giving tot-peso-tot
                                             
              if art-cod-doganale(1:4) = "2710"
                 if pen-si-sdoppia-riga
                    compute tot-peso-utf-3403 =
                            tot-peso-utf-3403 + 
                          ( ror-prg-peso of rordini * 
                            ror-qta      of rordini )
                 else                                                     
                    compute tot-peso-utf-2710 =
                            tot-peso-utf-2710 + 
                          ( ror-prg-peso of rordini * 
                            ror-qta      of rordini )
                 end-if
              else
                 compute tot-peso-non-utf =
                         tot-peso-non-utf + 
                       ( ror-prg-peso of rordini * 
                         ror-qta      of rordini )
              end-if
              compute tot-peso-tot =
                      tot-peso-tot + 
                    ( ror-prg-peso of rordini * ror-qta of rordini )
              move art-unita-di-misura  to st-udm
              move ror-qta of rordini   to ror-qta-z st-qta

              evaluate ror-qta-z(1:1)
              when 1 move "A" to st-dec-qta(1:1)
              when 2 move "E" to st-dec-qta(1:1)
              when 3 move "G" to st-dec-qta(1:1)
              when 4 move "H" to st-dec-qta(1:1)
              when 5 move "M" to st-dec-qta(1:1)
              when 6 move "P" to st-dec-qta(1:1)
              when 7 move "S" to st-dec-qta(1:1)
              when 8 move "T" to st-dec-qta(1:1)
              when 9 move "K" to st-dec-qta(1:1)
              end-evaluate

              evaluate ror-qta-z(2:1)
              when 0 move "Z" to st-dec-qta(2:1)
              when 1 move "A" to st-dec-qta(2:1)              
              when 2 move "E" to st-dec-qta(2:1)
              when 3 move "G" to st-dec-qta(2:1)
              when 4 move "H" to st-dec-qta(2:1)
              when 5 move "M" to st-dec-qta(2:1)
              when 6 move "P" to st-dec-qta(2:1)
              when 7 move "S" to st-dec-qta(2:1)
              when 8 move "T" to st-dec-qta(2:1)
              when 9 move "K" to st-dec-qta(2:1)
              end-evaLuate
              
              evaluate ror-qta-z(3:1)
              when 0 move "Z" to st-dec-qta(3:1)
              when 1 move "A" to st-dec-qta(3:1)               
              when 2 move "E" to st-dec-qta(3:1)
              when 3 move "G" to st-dec-qta(3:1)
              when 4 move "H" to st-dec-qta(3:1)
              when 5 move "M" to st-dec-qta(3:1)
              when 6 move "P" to st-dec-qta(3:1)
              when 7 move "S" to st-dec-qta(3:1)
              when 8 move "T" to st-dec-qta(3:1)
              when 9 move "K" to st-dec-qta(3:1)
              end-evaluate
              
              evaluate ror-qta-z(3:1)
              when 0 move "Z" to st-dec-qta(4:1)
              when 1 move "A" to st-dec-qta(4:1)               
              when 2 move "E" to st-dec-qta(4:1)
              when 3 move "G" to st-dec-qta(4:1)
              when 4 move "H" to st-dec-qta(4:1)
              when 5 move "M" to st-dec-qta(4:1)
              when 6 move "P" to st-dec-qta(4:1)
              when 7 move "S" to st-dec-qta(4:1)
              when 8 move "T" to st-dec-qta(4:1)
              when 9 move "K" to st-dec-qta(4:1)
              end-evaluate
              
              evaluate ror-qta-z(5:1)
              when 0 move "Z" to st-dec-qta(5:1)
              when 1 move "A" to st-dec-qta(5:1)               
              when 2 move "E" to st-dec-qta(5:1)
              when 3 move "G" to st-dec-qta(5:1)
              when 4 move "H" to st-dec-qta(5:1)
              when 5 move "M" to st-dec-qta(5:1)
              when 6 move "P" to st-dec-qta(5:1)
              when 7 move "S" to st-dec-qta(5:1)
              when 8 move "T" to st-dec-qta(5:1)
              when 9 move "K" to st-dec-qta(5:1)
              end-evaluate
              
              evaluate ror-qta-z(6:1)
              when 0 move "Z" to st-dec-qta(6:1)
              when 1 move "A" to st-dec-qta(6:1)               
              when 2 move "E" to st-dec-qta(6:1)
              when 3 move "G" to st-dec-qta(6:1)
              when 4 move "H" to st-dec-qta(6:1)
              when 5 move "M" to st-dec-qta(6:1)
              when 6 move "P" to st-dec-qta(6:1)
              when 7 move "S" to st-dec-qta(6:1)
              when 8 move "T" to st-dec-qta(6:1)
              when 9 move "K" to st-dec-qta(6:1)
              end-evaluate
              
              evaluate ror-qta-z(7:1)
              when 0 move "Z" to st-dec-qta(7:1)
              when 1 move "A" to st-dec-qta(7:1)               
              when 2 move "E" to st-dec-qta(7:1)
              when 3 move "G" to st-dec-qta(7:1)
              when 4 move "H" to st-dec-qta(7:1)
              when 5 move "M" to st-dec-qta(7:1)
              when 6 move "P" to st-dec-qta(7:1)
              when 7 move "S" to st-dec-qta(7:1)
              when 8 move "T" to st-dec-qta(7:1)
              when 9 move "K" to st-dec-qta(7:1)
              end-evaluate
              
              evaluate ror-qta-z(8:1)
              when 0 move "Z" to st-dec-qta(8:1)
              when 1 move "A" to st-dec-qta(8:1)               
              when 2 move "E" to st-dec-qta(8:1)
              when 3 move "G" to st-dec-qta(8:1)
              when 4 move "H" to st-dec-qta(8:1)
              when 5 move "M" to st-dec-qta(8:1)
              when 6 move "P" to st-dec-qta(8:1)
              when 7 move "S" to st-dec-qta(8:1)
              when 8 move "T" to st-dec-qta(8:1)
              when 9 move "K" to st-dec-qta(8:1)
              end-evaluate

              call "C$JUSTIFY" using st-dec-qta, "R"

              initialize line-riga
              |23/05/2012
              if ror-si-blister of rordini
                 move spaces to st-codart-cli
              end-if                            

              move "@#11" to line-riga
              perform STAMPA-RIGA
             
              |23/05/2012                       
              move struttura-stampa to line-riga
              perform STAMPA-RIGA
              add 1 to WrittenRows
 
OMAGGI        if ror-qta-omaggi of rordini not = 0
OMAGGI           perform SALTO-PAGINA
                 move "@#12" to line-riga
                 perform STAMPA-RIGA
OMAGGI           move ror-qta-omaggi of rordini to st-qta-oma
OMAGGI           move st-riga-omaggi to line-riga
OMAGGI           perform STAMPA-RIGA
OMAGGI           add 1 to WrittenRows
OMAGGI        end-if

              if stbolle-stampa perform AGGIORNA-PROGMAG end-if
           end-perform.

      ***---
       AGGIORNA-PROGMAG.
           if gia-fatto-progmag exit paragraph end-if.
           set gia-fatto-progmag to true.
           if tca-no-movim-giac and
              tca-no-movim-imp  and
              tca-no-movim-ord  and
              tca-no-giac-bloc
              continue
           else
              initialize link-wprogmag
              set link-update-um      to true
              set link-update-peso    to false
              set link-update-valore  to false
              move "0000000000000000" to link-array
              move  1                 to multiplyer(1)  |Agisco sulla giacenza dinamica
              move -1                 to multiplyer(2)  |Storno l'impegnato
              move  1                 to multiplyer(15) |Agisco sulla giacenza dinamica bloccata
              set  link-update        to true
              move ror-prg-chiave of rordini to link-key
              move tor-causale        to link-causale
              move ror-qta of rordini to link-valore
              move link-user          of stbolle-linkage         
                to link-user          of link-wprogmag
              call   "wprogmag"    using link-wprogmag
              cancel "wprogmag"
      *****        move ror-prg-chiave of rordini to prg-chiave
      *****        read progmag no lock
      *****        if prg-impegnato < 0
      *****           perform 5 times
      *****              display message 
      *****                "EVASIONE: " tor-numero
      *****         x"0d0a""ART : " prg-cod-articolo
      *****         x"0d0a""MAG : " prg-cod-magazzino
      *****         x"0d0a""IMB : " prg-tipo-imballo
      *****         x"0d0a""PESO: " prg-peso
      *****         x"0d0a""IMP : " prg-impegnato
      *****         x"0d0a""FROM: STBOLLE-P"
      *****         x"0d0a"
      *****         x"0d0a""CONTATTARE ASSISTENZA!!!"
      *****                        title titolo
      *****                         icon 3
      *****           end-perform
      *****        end-if
      *****        if prg-impegnato < ( prg-imp-GDO + prg-imp-TRAD )
      *****           perform 5 times
      *****              display message 
      *****                 "EVASIONE: " tor-numero
      *****          x"0d0a""ART :  " prg-cod-articolo
      *****          x"0d0a""MAG :  " prg-cod-magazzino
      *****          x"0d0a""IMB :  " prg-tipo-imballo
      *****          x"0d0a""PESO:  " prg-peso
      *****          x"0d0a""IMP :  " prg-impegnato
      *****          x"0d0a""IMP G: " prg-imp-GDO
      *****          x"0d0a""IMP T: " prg-imp-TRAD
      *****          x"0d0a""FROM:  STBOLLE-P"
      *****          x"0d0a"
      *****          x"0d0a""CONTATTARE ASSISTENZA!!!"
      *****                        title titolo
      *****                         icon 3
      *****           end-perform
      *****        end-if
           end-if.

      ***---
       DESCRIZIONE-IMBALLO.
           initialize st-imb.
           inspect ror-des-imballo of rordini
                   replacing trailing spaces by low-value.

           move ror-qta-imballi of rordini to imballi-x.
           inspect imballi-x replacing leading x"30" by x"20".
           call "C$JUSTIFY" using imballi-x, "L".
           inspect imballi-x replacing trailing spaces by low-value.

BLISTR     if ror-si-blister of rordini
BLISTR        move ror-des-imballo of rordini to st-imb
BLISTR        inspect st-imb replacing trailing low-value by spaces
           else
              string ror-des-imballo of rordini delimited low-value
                     imballi-x       delimited low-value
                     "x"             delimited size
                     art-udm-imballo delimited size
                     into st-imb
              end-string
           end-if.

      ***---
       STAMPA-SEGUE.
           set stampa--segue to true. 

           move PagePerBolla  to st-num-page.

           move "@#77" to line-riga.
           perform STAMPA-RIGA.


           if tor-contrassegno-no
              move st-riga-segue to line-riga
              perform STAMPA-RIGA
           else 
              move st-num-page           to st-num-page-contras
              move st-riga-segue-contras to line-riga
              perform STAMPA-RIGA

              move "@<0049" to line-riga
              perform STAMPA-RIGA 
              move "@+3" to line-riga
              perform STAMPA-RIGA
                            
              inspect contras-note-bolla 
                      replacing trailing spaces by low-value
              initialize line-riga
              string  contras-note-bolla delimited low-value
                      " "                delimited size
                      tot-bolla-x        delimited low-value
                 into line-riga
              end-string
              perform STAMPA-RIGA
                                  
              move "@-3" to line-riga
              perform STAMPA-RIGA     
              move "@>0007" to line-riga
              perform STAMPA-RIGA 
           end-if.

           if esiste-note move 20 to n-vuote
           else           move 17 to n-vuote
           end-if.

           add n-vuote-from-divisorio-master to n-vuote
           perform RIGHE-VUOTE.

      ***---
       STAMPA-NOTE-TOTALI.   
           move "@#77" to line-riga.
           perform STAMPA-RIGA.     
                 
           subtract WrittenRows from RowsPerPage giving n-vuote.
           perform RIGHE-VUOTE.     
              
           move 0 to st-tot-peso-utf-2710
                     st-tot-peso-utf-3403
                     st-tot-peso-non-utf st-tot-peso-tot
           initialize st-pag.

           if tor-contrassegno-no
              move PagePerBolla to PagePerBollaEdit
              string "*Tot. Pag. "    delimited size
                     PagePerBollaEdit delimited size
                     " - FINE*"       delimited size
                     into st-pag
              end-string
              move st-riga-pag to line-riga
              perform STAMPA-RIGA
           else                  
              move PagePerBolla to PagePerBollaEdit
              string "*Tot. Pag. "    delimited size
                     PagePerBollaEdit delimited size
                     " - FINE*"       delimited size
                     into st-pag-contras
              end-string
              move st-riga-pag-contras to line-riga
              perform STAMPA-RIGA

              move "@<0049" to line-riga
              perform STAMPA-RIGA 
              move "@+3" to line-riga
              perform STAMPA-RIGA
              inspect contras-note-bolla 
                      replacing trailing spaces by low-value
              initialize line-riga
              string  contras-note-bolla delimited low-value
                      " "                delimited size
                      tot-bolla-x        delimited low-value
                 into line-riga
              end-string
              perform STAMPA-RIGA
                                  
              move "@-3" to line-riga
              perform STAMPA-RIGA     
              move "@>0007" to line-riga
              perform STAMPA-RIGA 
           end-if.

           if esiste-note      
              perform AGGIUNGI-GG-DATA-CONSEGNA
              move  tor-note1           to st-note-1
              if tor-data-note1 not = 0
                 move  tor-data-note1(1:4) to st-note-data(7:4)
                 move  "/"                 to st-note-data(6:1)
                 move  tor-data-note1(5:2) to st-note-data(4:2)
                 move  "/"                 to st-note-data(3:1)
                 move  tor-data-note1(7:2) to st-note-data(1:2)
              else
                 move spaces to st-note-data
              end-if
                                          
              move  tot-colli to st-tot-colli
              initialize line-riga
              move st-riga-piede-1 to line-riga
              perform STAMPA-RIGA              

              move  tor-note2 to st-note-2-1
              move  tor-note3 to st-note-2-2
              move  tor-note4 to st-note-2-3
              initialize line-riga
              move st-riga-piede-2 to line-riga
              perform STAMPA-RIGA

              move  tor-note-bolla-1 to st-note-3-1
              move  tor-note-bolla-2 to st-note-3-2
              initialize line-riga
              move st-riga-piede-3 to line-riga
              perform STAMPA-RIGA
              
           end-if.       
      
           move 1 to n-vuote.
           perform RIGHE-VUOTE.
      
           |X sempre su mittente
           if st-mit = "X"     
              move "@#13" to line-riga
              perform STAMPA-RIGA       
              move "@<0010" to line-riga
              perform STAMPA-RIGA 
              move st-mit to line-riga
              perform STAMPA-RIGA
           end-if.       
           if st-dest = "X"     
              move "@#14" to line-riga   
              move "@#13" to line-riga
              perform STAMPA-RIGA       
              move "@<0010" to line-riga
              perform STAMPA-RIGA 
              move st-dest to line-riga
              perform STAMPA-RIGA
           end-if.          
           if st-vet = "X"     
              move "@#15" to line-riga   
              move "@#13" to line-riga
              perform STAMPA-RIGA
              move "@<0010" to line-riga
              perform STAMPA-RIGA 
              move st-vet to line-riga
              perform STAMPA-RIGA
           end-if.       

           move "@#21" to line-riga.
           perform STAMPA-RIGA.
           
           move  tot-peso-utf-2710    to st-tot-peso-utf-2710.  
           initialize line-riga.
           move "@<0040" to line-riga.
           perform STAMPA-RIGA.                                 
           move st-riga-utf-2710 to line-riga.
           perform STAMPA-RIGA. 
           move  tot-peso-utf-3403 to st-tot-peso-utf-3403.
           initialize line-riga.
           move "@>0010" to line-riga.
           perform STAMPA-RIGA.
           move st-riga-utf-3403 to line-riga.
           perform STAMPA-RIGA. 
           move "@>0010" to line-riga.
           perform STAMPA-RIGA.
           move  tot-peso-non-utf to st-tot-peso-non-utf.
           initialize line-riga.
           move st-riga-non-utf to line-riga.
           perform STAMPA-RIGA. 
           move "@>0020" to line-riga.
           perform STAMPA-RIGA.
           move  tot-peso-tot to st-tot-peso-tot.
           initialize line-riga.
           move st-riga-tot to line-riga.
           perform STAMPA-RIGA. 
           move "@<0065" to line-riga.
           perform STAMPA-RIGA.    

           move "@#77" to line-riga.
           perform STAMPA-RIGA.

           move 4 to n-vuote.
           perform RIGHE-VUOTE.
                                                    
           move "@>0030" to line-riga.
           perform STAMPA-RIGA.
           initialize line-riga.
           move st-vettore-estesa-1 to line-riga.
           perform STAMPA-RIGA.      

           initialize line-riga.
           move st-vettore-estesa-2 to line-riga.
           perform STAMPA-RIGA.

           initialize line-riga.
           move st-vettore-estesa-2b to line-riga.
           perform STAMPA-RIGA.        

           if stbolle-giro = 2
              initialize tcb-rec replacing numeric data by zeroes
                                      alphanumeric data by spaces
              move tor-vettore to tcb-vet-codice
              read tcb no lock 
                   invalid continue
               not invalid
                   add 1 to tcb-tot-bolle
                   compute tcb-tot-kg = 
                           tcb-tot-kg +
                           tot-peso-tot
                   rewrite tcb-rec
              end-read
           end-if.

           move 6 to n-vuote.     
           perform RIGHE-VUOTE.

      ***---
       ORDINE-IN-BOLLA.
           move tor-anno to eva-anno.
           if eva-da = 0
              move tor-numero to eva-da
           end-if.
           move tor-numero to eva-a.

           evaluate tcl-serie-bolle
           when 1 |stbolle-gdo
                add 1 to con-num-bolle-gdo
                move con-num-bolle-gdo to tor-num-bolla
           when 2 |stbolle-MV
                add 1 to con-num-bolle-MV
                move con-num-bolle-MV to tor-num-bolla
           when 3 |stbolle-AT
                add 1 to con-num-bolle-AT
                move con-num-bolle-AT to tor-num-bolla
           end-evaluate.        

           if tor-num-bolla < stbolle-prima-bolla
              move tor-num-bolla to stbolle-prima-bolla
           end-if.                                     
           move tor-num-bolla to stbolle-ultima-bolla.

           move stbolle-anno to tor-anno-bolla.
           move stbolle-data to tor-data-bolla.

           accept como-data  from century-date.
           accept como-ora   from time.
           move como-data    to tor-data-ultima-modifica.
           move como-ora     to tor-ora-ultima-modifica.
           move link-user    of stbolle-linkage
                             to tor-utente-ultima-modifica.

           set tor-no-agg-contab to true.

           accept tor-data-bolla-effettiva from century-date
           
           if tor-data-ordine > tor-data-bolla
              move tor-data-bolla to tor-data-ordine
           end-if.
      *****     accept tor-data-ultima-modifica from century-date
      *****     accept tor-ora-ultima-modifica  from time
      *****     move user-codi to tor-utente-ultima-modifica

           set tor-fatt-si-prenotata to true.

           rewrite tor-rec   invalid continue end-rewrite.
           unlock  tordini   all records.

           accept como-data  from century-date.
           accept como-ora   from time.
           move   como-data  to con-data-ultima-modifica.
           move   como-ora   to con-ora-ultima-modifica.
           move   link-user  of stbolle-linkage
                             to con-utente-ultima-modifica.

           evaluate tcl-serie-bolle
           when 1 |stbolle-gdo
                move stbolle-data to con-ult-stampa-bolle-GDO
           when 2 |stbolle-MV
                move stbolle-data to con-ult-stampa-bolle-MV
           when 3 |stbolle-AT
                move stbolle-data to con-ult-stampa-bolle-AT
           end-evaluate.
           rewrite con-rec   invalid continue end-rewrite.


      ***---
       BOLLA-MULTIPLA.
           set ControlloCausali to true.
           |Dopo lo split torno sulla prima (quella di testata)
           move sav-chiave to tor-chiave.

           add 1           to idx-original.
           move tor-chiave to ordine-original(idx-original).

           |Essendo appena stato letto con lock, aperto per
           |lo split nel frattempo nessuno sarà in modifica
           read tordini lock invalid continue end-read.
           move tor-chiave to tor-ordine-testa. 

           perform ORDINE-IN-BOLLA.
           move tor-num-bolla  to rlt-num-bolla.
             
           if stbolle-giro = 1
              move " " to st-dest 
              move "X" to st-vet 
              move " " to st-mit
              perform STAMPA-DATI    
              move " " to st-vet 
              move " " to st-mit
              move "X" to st-dest
              perform STAMPA-DATI
           else                      
              move " " to st-vet
              move " " to st-dest
              move "X" to st-mit
              perform STAMPA-DATI
           end-if.

           move tor-chiave to rlt-chiave.
           read reltor no lock
                invalid continue
            not invalid
                move tor-anno-bolla to rlt-anno-bolla
                move tor-num-bolla  to rlt-num-bolla
                rewrite rlt-rec invalid continue end-rewrite

                move 1 to idx-coll
                perform until 1 = 2
                   if rlt-anno-s(idx-coll)   = 0 or
                      rlt-numero-s(idx-coll) = 0
                      exit perform
                   end-if
                   move rlt-chiave-split(idx-coll) to tor-chiave
                   |Essendo appena creato nessuno ci andrà in modifica
                   |Per sicurezza la blocco comunque senza però 
                   |effettuare nessun controllo sul fatto che sia libera
                   read tordini lock invalid continue end-read
                   move rlt-chiave to tor-ordine-testa
                   perform ORDINE-IN-BOLLA  
                   if stbolle-giro = 1
                      move " " to st-dest 
                      move "X" to st-vet 
                      move " " to st-mit
                      perform STAMPA-DATI    
                      move " " to st-vet 
                      move " " to st-mit
                      move "X" to st-dest
                      perform STAMPA-DATI
                   else                      
                      move " " to st-vet
                      move " " to st-dest
                      move "X" to st-mit
                      perform STAMPA-DATI
                   end-if
                   move tor-anno-bolla to rlt-anno-b-s(idx-coll)
                   move tor-num-bolla  to rlt-num-b-s(idx-coll)
                   rewrite rlt-rec invalid continue end-rewrite
                   add 1 to idx-coll
                end-perform
                move 0 to tor-anno-bolla tor-data-bolla tor-num-bolla
                set tor-bolla-si-prenotata to true
                move sav-chiave to tor-chiave
                start tordini key >= k-stbolle
           end-read.

      ***---
       SALTO-PAGINA.
           if WrittenRows = RowsPerPage
              perform STAMPA-SEGUE  

              perform STAMPA-TESTA  
   
              move "@<0010" to line-riga
              perform STAMPA-RIGA

              move 1 to n-vuote 
              perform RIGHE-VUOTE

              move 0 to WrittenRows
              add  1 to PagePerBolla

      *    luciano
              if stampa-corpo
                 if ror-numero-master of rordini not = 0
                    perform DIVISORIO-MASTER
                 end-if
              end-if
      *    luciano fine
           end-if.

      ***---
       RIGHE-VUOTE.
           perform n-vuote times
              evaluate idx-stampante
              when 1 write line-riga1 from spaces after 1
              when 2 write line-riga2 from spaces after 1
              when 3 write line-riga3 from spaces after 1
              when 4 write line-riga4 from spaces after 1
              when 5 write line-riga5 from spaces after 1
              end-evaluate
           end-perform.

      ***---
       STAMPA-RIGA.
           write line-riga
           evaluate idx-stampante
           when 1 write line-riga1 from line-riga
           when 2 write line-riga2 from line-riga
           when 3 write line-riga3 from line-riga
           when 4 write line-riga4 from line-riga
           when 5 write line-riga5 from line-riga
           end-evaluate.

      ***--- STATO ORDINE XXX
       AGGIORNA-MASTER. 
           perform varying idx-master from 1 by 1 
                     until idx-master > tot-master
              move el-ordine-m(idx-master) to mto-chiave
              perform AGGIORNA-STATO-MASTER
           end-perform.
           |||XXX

      ***---
       CLOSE-FILES.
           close tordini  clienti mrordini assorcli progmag
                 destini  tvettori rordini mtordini tmarche timposte
                 articoli tcontat  tcaumag ttipocli
                 tnomen   listini reltor  rordini1 param
                 tparamge prodener tpromo  rpromo tscorte tivaese.

           close lineseq.

           if stbolle-giro = 2
              close       tcb
              delete file tcb
           end-if.

           call "C$DELETE" using path-globale.

      ***---
       EXIT-PGM.
           move wstampa to stbolle-path.
           if idx-original > 0
              perform ST-DIVISIONE-CRT
           end-if.

           if ControlloCausali
              call   "evaomag" using eva-anno eva-da eva-a
              cancel "evaomag"
           end-if.
           goback.

      ***---
       ST-DIVISIONE-CRT.
           initialize wstampa
           accept  wstampa   from environment "PATH-ST"
           accept  como-data from century-date
           accept  como-ora  from time
           inspect wstampa   replacing trailing spaces by low-value
           string wstampa    delimited by low-value
                  "split"    delimited by size
                  "_"        delimited by size
                  como-data  delimited by size
                  "_"        delimited by size
                  como-ora   delimited by size
                  ".txt"     delimited by size
                  into wstampa
           end-string
           open output lineseq
           write line-riga from spaces
           move "DIVISIONE ORDINI > 500 KG UTF" to line-riga
           write line-riga
           write line-riga from spaces
           perform varying idx from 1 by 1 until idx > idx-original
              move ordine-original(idx) to rlt-chiave
              read reltor no lock invalid continue end-read
              move rlt-numero       to rlt-numero-ed
              move rlt-numero-s(1)  to rlt-numero-1
              move rlt-numero-s(2)  to rlt-numero-2
              move rlt-numero-s(3)  to rlt-numero-3
              move rlt-numero-s(4)  to rlt-numero-4
              move rlt-numero-s(5)  to rlt-numero-5
              move rlt-numero-s(6)  to rlt-numero-6
              move rlt-numero-s(7)  to rlt-numero-7
              move rlt-numero-s(8)  to rlt-numero-8
              move rlt-numero-s(9)  to rlt-numero-9
              move rlt-numero-s(10) to rlt-numero-10
              move rlt-numero-s(11) to rlt-numero-11
              move rlt-numero-s(12) to rlt-numero-12
              move rlt-numero-s(13) to rlt-numero-13
              move rlt-numero-s(14) to rlt-numero-14
              move rlt-numero-s(15) to rlt-numero-15
              initialize line-riga
              string "ORDINE ORIGINALE: " delimited size
                     rlt-numero-ed        delimited size
                     " in ---> "          delimited size
                     rlt-numero-ed        delimited size
                     " "                  delimited size
                     rlt-numero-1         delimited size
                     rlt-numero-2         delimited size
                     rlt-numero-3         delimited size
                     rlt-numero-4         delimited size
                     rlt-numero-5         delimited size
                     rlt-numero-6         delimited size
                     rlt-numero-7         delimited size
                     rlt-numero-8         delimited size
                     rlt-numero-9         delimited size
                     rlt-numero-10        delimited size
                     rlt-numero-11        delimited size
                     rlt-numero-12        delimited size
                     rlt-numero-13        delimited size
                     rlt-numero-14        delimited size
                     rlt-numero-15        delimited size
                     into line-riga
              end-string
              write line-riga
           end-perform
           close       lineseq
           move wstampa to stbolle-split.


      ***--- DUMMY: NON TOCCARE!!!
       AGGIORNA-IMPEGNATO-MASTER.


      ***---
       DIVISORIO-MASTER.
           move 0 to n-vuote-from-divisorio-master.

           if (WrittenRows + 3) > RowsPerPage
              compute n-vuote = RowsPerPage - WrittenRows
              perform RIGHE-VUOTE
              move RowsPerPage  to WrittenRows
              perform SALTO-PAGINA
           end-if

           if WrittenRows > 0
              write line-riga from spaces
              evaluate idx-stampante
              when 1 write line-riga1 from spaces
              when 2 write line-riga2 from spaces
              when 3 write line-riga3 from spaces
              when 4 write line-riga4 from spaces
              when 5 write line-riga5 from spaces
              end-evaluate
              add 1 to WrittenRows
           end-if.

           move ror-anno-master   of rordini to st-om-anno.
           move ror-numero-master of rordini to st-om-numero.
           call "C$JUSTIFY" using st-om-numero "L".

           move ror-anno-master   of rordini to mto-anno.
           move ror-numero-master of rordini to mto-numero.

           read mtordini no lock invalid continue end-read.

           move mto-num-ord-cli to st-om-numordcli.

           string mto-data-ordine(7:2) delimited size
                  "/"                  delimited size
                  mto-data-ordine(5:2) delimited size
                  "/"                  delimited size
                  mto-data-ordine(1:4) delimited size
                  into st-om-datacli
           end-string.

           move "@#77" to line-riga.
           perform STAMPA-RIGA.
           
           move st-ordine-master   to line-riga.
           perform STAMPA-RIGA.
           add 1 to WrittenRows.
           move ror-chiave-ordine-testa of rordini                    
             to old-ror-chiave-ordine-testa.

      ***---
       CREA-PDF.                                                  
           accept DestFile from environment "INVIO_SOL_PATH_PDF".
           accept como-data from century-date.
           accept como-ora  from time.
      *    tolgo l'eventuale barra finale
      *****     inspect DestFile replacing trailing spaces by low-value.
      *****     initialize cont.
      *****     inspect DestFile tallying cont
      *****             for characters before low-value.
      *****     if DestFile(cont:1) = "\" 
      *****        move low-value  to DestFile(cont:1)
      *****     end-if.
      *****     inspect DestFile replacing trailing low-value by spaces.   
                                                                  
      *     Come convenzione uso stbolle-data come numero di bolla
           string stbolle-data       delimited size
                  "__"               delimited size
                  como-data(7:2)     delimited size
                  "-"                delimited size
                  como-data(5:2)     delimited size
                  "-"                delimited size
                  como-data(1:4)     delimited size
                  "_"                delimited size
                  como-ora(1:2)      delimited size
                  "-"                delimited size   
                  como-ora(3:2)      delimited size
                  "-"                delimited size   
                  como-ora(5:2)      delimited size
                  into NomeFile
           end-string.

           set settaPDF-setta to true.

           move NomeFile  to settaPDF-nome-file.
           move DestFile  to settaPDF-percorso.
           call   "settaPDF2" using settaPDF-linkage.
           cancel "settaPDF2".        

      ***---
       ASPETTA-PDF.
      *****     move link-path to como-nome-file.
      *****     move 0 to cont.
      *****     inspect como-nome-file 
      *****             tallying cont for characters before ")".
      *****     move ".pdf " to como-nome-file(cont + 1: 5).
      *****
      *****     set trovato to false.
      *****     perform 60 times
      *****        CALL "C$FILEINFO" USING link-path,
      *****                                file-info, 
      *****                         GIVING status-code
      *****        if status-code = 0
      *****           set trovato to true
      *****           exit perform
      *****        else
      *****           CALL "C$FILEINFO" USING como-nome-file,
      *****                                   file-info, 
      *****                            GIVING status-code
      *****           if status-code = 0
      *****              move como-nome-file to link-path
      *****              set trovato to true
      *****              exit perform
      *****           end-if
      *****        end-if
      *****        call "c$sleep" using 1
      *****     end-perform.
      *****
      *****     if trovato
      *****        move 0  to old-size
      ******       aspetto finchè non esiste fisicamente il file
      *****        move 99 to minuti-partenza
      *****        perform until 1 = 2
      *****           CALL "C$FILEINFO" USING link-path,
      *****                                   file-info, 
      *****                            GIVING status-code
      *****        
      *****           if status-code = 0
      *****              if FILE-SIZE not = 0
      *****                 if FILE-SIZE = old-size
      *****                    exit perform
      *****                 else
      *****                    move FILE-SIZE to old-size
      *****                    call "c$sleep" using 1
      *****                 end-if
      *****              end-if
      *****           else
      *****              perform TIME-OUT
      *****              if time-out-exit
      *****                 exit perform
      *****              end-if
      *****           end-if
      *****        
      *****        end-perform
      *****     end-if.

           set settaPDF-resetta   to true.
           call   "settaPDF2" using settaPDF-linkage
           cancel "settaPDF2".  

           if settaPDF-OK
              initialize settaPDF-nome-file                          
              inspect DestFile replacing trailing spaces by low-value
              inspect NomeFile replacing trailing spaces by low-value
              string DestFile  delimited low-value
                     NomeFile  delimited low-value
                     ".pdf"    delimited size
                into settaPDF-nome-file
              end-string
              move settaPDF-nome-file to wstampa
           end-if.  

      ***---
       RIEPILOGO-CORRIERI.
           accept  wstampa     from environment "PATH-ST".
           accept  como-data   from century-date.
           accept  como-ora    from time.
           inspect wstampa     replacing trailing spaces by low-value.
           string wstampa       delimited by low-value
                  "tcb"         delimited by size
                  "_"           delimited by size
                  como-data     delimited by size
                  "_"           delimited by size
                  como-ora      delimited by size
                  ".txt"        delimited by size
                  into wstampa
           end-string.
           move wstampa to stbolle-path-riepilogo.
           open output line-riep. 
                       
           move 999 to cont.                  
           move low-value to tcb-rec.
           start tcb key >= tcb-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read tcb next at end exit perform end-read
                    if tcb-num-bolla = 0  
                       if cont = 999
                          move 0 to cont     
                       end-if  
       
                       perform STAMPA-PRESA-IN-CARICO
                                                             
                       move 1 to tcb-num-bolla tcb-data-bolla
                       move vet-codice to tcb-vet-codice
                       start tcb key >= tcb-chiave
                             invalid continue
                       end-start                     
                       move tot-peso-ed  to tcb-tot-kg
                       move tot-bolle-ed to tcb-tot-bolle
       
                       perform STAMPA-PRESA-IN-CARICO

                    end-if
                 end-perform
           end-start.
           close line-riep.

      ***---
       STAMPA-PRESA-IN-CARICO.    
           move "@#99" to lr-riga.
           write lr-riga.

           move "@#31" to lr-riga.
           write lr-riga.
           
           move "@+7" to lr-riga
           write lr-riga
                                    
           move "@B+" to lr-riga
           write lr-riga           

           move "@<0300" to lr-riga
           write lr-riga           
           
           initialize lr-riga
           string "                    " 
                       delimited size
                  "DICHIARAZIONE DI PRESA IN CARICO BOLLE"
                       delimited size
             into lr-riga
           end-string
           write lr-riga              
           write lr-riga from spaces  
           write lr-riga from spaces     
                           
           move "@-7" to lr-riga
           write lr-riga
           
           move "@B-" to lr-riga
           write lr-riga   
           
           initialize lr-riga
           string "  STAMPATO IL: " delimited size
                  como-data(7:2)    delimited size
                  "/"               delimited size
                  como-data(5:2)    delimited size
                  "/"               delimited size
                  como-data(1:4)    delimited size
                  " ALLE: "         delimited size
                  como-ora(1:2)     delimited size
                  ":"               delimited size
                  como-ora(3:2)     delimited size
             into lr-riga
           end-string
           write lr-riga              
                  
           initialize lr-riga
           string "  LUBEX SPA "              delimited size
                  "VIA G. DI VITTORIO 13/15 " delimited size
                  " - 20090 VIMODRONE - "     delimited size
                  "(MILANO - ITALY)     "     delimited size
             into lr-riga
           end-string
           write lr-riga               
           write lr-riga from spaces    
                           
           move "@+7" to lr-riga
           write lr-riga
           
           move "@B+" to lr-riga
           write lr-riga   

           move tcb-vet-codice to vet-codice
           read tvettori 
                invalid move spaces to st-vettore
                                       st-vettore-estesa-1
                                       st-vettore-estesa-2
                
                if vet-codice = 0
                   move "DIRETTO" to st-vet-ragsoc
                end-if
            not invalid 
                initialize st-vet-ragsoc
                inspect vet-descrizione 
                        replacing trailing spaces 
                        by low-value
                string vet-descrizione  delimited low-value
                       " - "            delimited size
                       vet-piva         delimited size
                       " - "            delimited size
                       vet-n-albo       delimited size
                       into st-vet-ragsoc
                end-string
                move vet-sigla       to st-vettore
                move vet-indirizzo   to st-vet-ind
           end-read
                                              
           move st-vettore-estesa-1 to lr-riga
           write lr-riga       
           write lr-riga from spaces      
           move st-vettore-estesa-2 to lr-riga
           write lr-riga       
           write lr-riga from spaces  
           write lr-riga from spaces      
                                     
           move "@-7" to lr-riga
           write lr-riga
                                 
           move "@B-" to lr-riga
           write lr-riga       
                          
           move tcb-tot-kg    to tot-peso-ed
           move tcb-tot-bolle to tot-bolle-ed
           initialize lr-riga
           string "   TOT. KG: "   delimited size
                  tot-peso-ed      delimited size
                  "   TOT. BOLLE " delimited size
                  tot-bolle-ed     delimited size
             into lr-riga
           end-string
           write lr-riga   
                               
           write lr-riga from spaces
           move "   LISTA BOLLE:" to lr-riga
           write lr-riga
           initialize lr-riga
           move 4 to CountChar
           move 0 to cont righe-bolle
           move 0 to righe-bolle.
           perform until 1 = 2
              read tcb next 
                   at end                       
                   read tcb previous
                   exit perform                 
              end-read
              if tcb-vet-codice not = vet-codice
                 read tcb previous
                 exit perform   
              end-if
              add 1 to cont
              move tcb-num-bolla to num-bolla-ed
              string tcb-data-bolla(7:2) delimited size
                     "/"                 delimited size
                     tcb-data-bolla(5:2) delimited size
                     "/"                 delimited size
                     tcb-data-bolla(1:4) delimited size
                into data-bolla-ed
              end-string
              initialize dati-bolla
              string num-bolla-ed  delimited size
                     " - "         delimited size
                     data-bolla-ed delimited size
                     " || "        delimited size
                into dati-bolla
              end-string
              move dati-bolla to lr-riga(CountChar:25)
              add 25 to CountChar
              if cont = 5
                 add 1 to righe-bolle
                 if righe-bolle > 25
                    move "@#99" to lr-riga
                    write lr-riga           
                    move "@#31" to lr-riga
                    write lr-riga
                    move 0 to righe-bolle
                 end-if
                 write lr-riga       
                 initialize lr-riga
                 move 4 to CountChar
                 move 0 to cont
              end-if
           end-perform
           if cont > 0
              if righe-bolle > 25
                 move "@#99" to lr-riga
                 write lr-riga                   
                 move "@#31" to lr-riga
                 write lr-riga           
                 move 0 to righe-bolle
              end-if
              write lr-riga  
           end-if
           if righe-bolle > 11
              move "@#99" to lr-riga        
              write lr-riga                  
              move "@#31" to lr-riga
              write lr-riga            
           end-if.
           write lr-riga from spaces
           move "   DA COMPILARE" to lr-riga
           write lr-riga                   
           write lr-riga from spaces
           move "   NOME:"   to lr-riga
           write lr-riga                
           write lr-riga from spaces    
           write lr-riga from spaces              
           move "   COGNOME: " to lr-riga
           write lr-riga      
           write lr-riga from spaces    
           write lr-riga from spaces              
           move "   TARGA: "   to lr-riga
           write lr-riga  
           write lr-riga from spaces    
           write lr-riga from spaces              
           move "   DATA / ORA: "   to lr-riga
           write lr-riga  
           write lr-riga from spaces    
           write lr-riga from spaces    
           move "   FIRMA: " to lr-riga
           write lr-riga.      

      ***---
       AGGIUNGI-GG-DATA-CONSEGNA.
           if tor-data-note1 = 0
              accept tor-data-note1 from century-date
              compute como-data = 
                      function integer-of-date (tor-data-note1)
              add tge-giorni-note-bolla to como-data
              compute tor-data-note1 = 
                      function date-of-integer (como-data)
           end-if.

      ***---
       PARAGRAFO-COPY.
           copy "aggiorna-stato-master.cpy". 
           copy "direziona-impegnato-common.cpy".
           copy "trova-su-assorcli.cpy".
           copy "trova-su-listini.cpy".
           copy "trova-parametro.cpy".
