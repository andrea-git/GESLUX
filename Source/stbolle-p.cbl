       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      stbolle-p.
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

       SELECT csvInput
           ASSIGN       TO path-csvInput
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE  IS SEQUENTIAL
           FILE STATUS  IS STATUS-csvInput.

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

      *(( XFD FILE = lineseq5 ))
       FD  csvInput.
       01 csvInput-riga        PIC  x(900).

       WORKING-STORAGE SECTION.
      * FILE DI COPY           
           copy "link-settaPDF.def".
           copy "acucobol.def".
           copy "acugui.def".
           copy "fonts.def".
           copy "PRT-ESC.def".
           copy "link-geslock.def".
           copy "link-wprogmag.def".
           copy "aggiorna-stato-master.def".
           copy "spooler.def".
           copy "selprint.lks".
           copy "versione-evasione.def".
           copy "trova-parametro.def".       
           
       01  filler             pic 9 value 0.
         88 ControlloCausali  value 1, false 0.
                                                    
       77  eva-anno                pic 9(4) value 0.
       77  eva-da                  pic 9(8) value 0.
       77  eva-a                   pic 9(8) value 0.
                                        
       77  stampante-albaoil       pic x(100).
       77  DestFile                pic x(256).
       77  NomeFile                pic x(256).                                              

      * FILE STATUS
           |||STATO ORDINE XXX
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
       77  status-csvInput         pic xx.
       77  wstampa                 pic x(256).
       77  path-csvInput           pic x(200).
       
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
           88  NoTXTPrincipale     value 1, false 0.
                                                    
       77  filler                  pic 9.
           88  trovato-art         value 1, false 0.
       77  filler                  pic 9 value 0.
           88  GeneraPdf         value 1, false 0.

       77  tot-master              pic 9(3) value 0.
       01  el-ordine-m             occurs 999 indexed by idx-master.
         05 el-anno-m              pic 9(4).
         05 el-numero-m            pic 9(8).
           |XXX            

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
         03 filler                 pic x(2).
         03 cli-riga-titolo-1      pic x(35).
         03 filler                 pic x(3).
         03 des-riga-titolo-1      pic x(40).

       01  st-riga-titolo-2.
         03 filler                 pic x(2).
         03 cli-riga-titolo-2      pic x(35).
         03 filler                 pic x(3).
         03 des-riga-titolo-2      pic x(35).

       01  st-riga-titolo-3.
         03 filler                 pic x(2).
         03 cli-riga-titolo-3.
           05 cli-st-cap           pic 9(5).
           05 filler               pic x.
           05 cli-st-localita      pic x(30).
         03 filler                 pic x(2).
         03 des-riga-titolo-3.
           05 des-st-localita      pic x(35).

       01  st-riga-titolo-4.
         03 filler                 pic x(40).
         03 des-st-cap             pic x(5).
         03 filler                 pic x(28).
         03 des-st-prov            pic xx.

       01  st-riga-vettore.
         03 filler                 pic x(29).
         03 st-vettore             pic x(3). 
         03 st-numord              pic z(6).

       01  st-riga-vettore-gordcvar.
         03 filler                 pic x(65).
         03 st-vettore-gv          pic x(3). 
         03 st-numord-gv           pic z(6).

       01  st-num-data.
         03 filler                 pic x(51).
         03 st-num-bolla           pic z(8).
         03 filler                 pic x(5).
         03 st-data-bolla          pic x(10).

       01  st-x.
         03 filler                 pic x(49).
         03 st-elimina             pic x(10).
         03 filler                 pic x(1).
         03 filler                 pic x(15)
                                   value "Bolle collegate".

       01 st-testa.
         03 filler                 pic x(2).
         03 st-cod-cli             pic z(5).
         03 filler                 pic x(3).
         03 st-num-ord-cli         pic x(10) justified right.
         03 filler                 pic x.
         03 st-data-ordine         pic x(8).
         03 filler                 pic x(1).
         03 st-causale             pic x(18).
         03 filler                 pic x(1).
         03 st-collegate           pic x(35).

       01  struttura-stampa.
         03 st-dati.
           05 st-cod-art           pic z(5).
           05 filler               pic x.
           05 st-cod-dog           pic x(8).
           05 filler               pic x(2).
           05 st-colli             pic z(5).
           05 filler               pic x(4).
           05 st-imb               pic x(14).
           05 filler               pic x(6).
           05 st-des-art           pic x(30).
           05 filler               pic x.
           05 st-codart-cli        pic x(10).
           05 filler               pic x.
           05 st-peso-utf          pic zzz9,999 blank zero.
           05 filler               pic x(3).
           05 st-udm               pic xx.
           05 filler               pic x(6).
           05 st-qta               pic zz.zzz.zzz.
           05 filler               pic x(7).
           05 st-dec-qta           pic x(8). 

       01  struttura-stampa-serie2.
         03 st-dati-s2.
           05 st-cod-art-s2        pic z(5).
           05 filler               pic x(5).
           05 st-cod-dog-s2        pic x(8).
           05 filler               pic x(4).
           05 st-colli-s2          pic z(5).
           05 filler               pic x(3).
           05 st-imb-s2            pic x(14).
           05 filler               pic x(5).
           05 st-des-art-s2        pic x(30).
           05 filler               pic x.
           05 st-codart-cli-s2     pic x(9).
           05 filler               pic x.
           05 st-peso-utf-s2       pic zzz9,999 blank zero.
           05 filler               pic x(2).
           05 st-udm-s2            pic xx.
           05 filler               pic x(2).
           05 st-qta-s2            pic zz.zzz.zzz.
           05 filler               pic x(7).
           05 st-dec-qta-s2        pic x(8). 

OMAGGI 01  st-riga-omaggi.
OMAGGI   03 filler                 pic x(80).
OMAGGI   03 filler                 pic x(14) value "DI CUI OMAGGIO".
OMAGGI   03 filler                 pic x(11).
OMAGGI   03 st-qta-oma             pic zz.zzz.zzz.

       01  st-riga-segue.
         03 filler                 pic x(54).
         03 filler                 pic x(5)  value "Pag. ".
         03 st-num-page            pic z(2).
         03 filler                 pic x(4)  value "  - ".
         03 filler                 pic x(6)  value "Segue ".
         03 filler                 pic x(5)  value "---->".

       01  st-riga-segue-gordcvar.
         03 filler                 pic x(108).
         03 filler                 pic x(5)  value "Pag. ".
         03 st-num-page-gv         pic z(2).
         03 filler                 pic x(4)  value "  - ".
         03 filler                 pic x(6)  value "Segue ".
         03 filler                 pic x(5)  value "---->".

       01  st-riga-piede-1.
         03 filler                 pic x(35).
         03 filler                 pic x(10) value "*******>".
         03 st-note-1              pic x(19).
         03 filler                 pic x.
         03 st-note-data           pic x(10).

       01  st-riga-piede-2.
         03 filler                 pic x.
         03 filler                 pic x(7) value "T.colli".
         03 st-tot-colli           pic z(5).
         03 filler                 pic x(32).
         03 st-note-2              pic x(30).

       01  st-riga-piede-3.
         03 filler                 pic x(45).
         03 st-note-3              pic x(30).

       01  st-riga-piede-4.
         03 filler                 pic x(45) value "TOTALE PEDANE".
         03 st-note-4              pic x(30).

       01  st-riga-piede-5.
         03 st-note-5              pic x(130).

       01  st-riga-piede-6.
         03 st-note-6              pic x(130).

       01  st-riga-vett-x.
         03 filler                 pic x(16).
         03 dicitura-vett          pic x(9).
         03 flag-vettore           pic x.
            88 riga-vettore        value "X" false space.

       01 st-riga-utf.
         03 filler                 pic x(62).
         03 st-tot-peso-utf        pic zzz.zzz.zz9,999.

       01  st-riga-non-utf.           
         03 filler                 pic x(62).
         03 st-tot-peso-non-utf    pic zzz.zzz.zz9,999.

       01  st-riga-tot.               
         03 filler                 pic x(62).
         03 st-tot-peso-tot        pic zzz.zzz.zz9,999.

       01 st-riga-utf-gordcvar.
         03 filler                 pic x(72).
         03 filler                 pic x(43)
            value "PRODOTTI SOGGETTI UTF KG.: ".
         03 st-tot-peso-utf-gv     pic zzz.zzz.zz9,999.

       01  st-riga-non-utf-gordcvar.           
         03 filler                 pic x(72).
         03 filler                 pic x(43) 
            value "PRODOTTI ALTRI KG.: ".
         03 st-tot-peso-non-utf-gv pic zzz.zzz.zz9,999.

       01  st-riga-tot-gordcvar.
         03 filler                 pic x(72).
         03 filler                 pic x(43) value "TOTALE PESO KG.: ".
         03 st-tot-peso-tot-gv     pic zzz.zzz.zz9,999.

       01  st-vettore-estesa-1.
         03 filler                 pic x(1).
         03 st-vet-ragsoc          pic x(72).

       01  st-vettore-estesa-1b.
         03 filler                 pic x(1).
         03 st-vet-piva-albo       pic x(65).
                                          
       01  st-vettore-estesa-2.
         03 filler                 pic x(1).
         03 st-vet-ind             pic x(40).  

       01  st-vettore-estesa-2b.
         03 filler                 pic x(1).
         03 st-vet-ind2            pic x(40).           

       01  st-ordine-master.
           03 filler               pic x(16) value "Ordine Cliente: ".
           03 st-om-numordcli      pic x(10).
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
       77  tot-peso-utf            pic 9(9)v999.
       77  tot-peso-non-utf        pic 9(9)v999.
       77  tot-peso-tot            pic 9(9)v999.
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
       77  LinkPgm                 pic x(20).
       77  como-bolla              pic 9(8).
       01  como-chiave.
         05 como-anno              pic 9(4).
         05 como-numero            pic 9(8).

       01  sav-chiave.
         05 sav-anno               pic 9(4).
         05 sav-numero             pic 9(8).

       77  righe-page-feed-x       pic x(3) value spaces.
       77  righe-page-feed         pic s99  value 0.

       77  n-vuote                 pic s99 value 0.
       77  n-vuote-from-divisorio-master pic 99 value 0.
       77  num-bolle-GDO           pic 9(8).
       77  num-bolle-AT            pic 9(8).

      * COSTANTI
       78  titolo                  value "Stampa Bolle".
       78  NumRigheConNote         value 14.
       78  NumRigheSenzaNote       value 16.

       78  NumRigheConNote-graf    value 36.
       78  NumRigheSenzaNote-graf  value 38.

       78  RigheVuoteIntestazione  value 19. |18 valore fisso salto pagina

LUBEXX***  Usate solo per la prima pagina come
LUBEXX***  scarrellamento fisso iniziale della carta

           |Usata per GDO
LUBEXX 78  RigheVuoteIntestazioneCompuprint  value 18.
LUBEXX     |18 misura stampante Lubex piano terra (Compuprint 9058 default)
           |Usata per MV+
LUBEXX 78  RigheVuoteIntestazioneEpsonDFX    value 16.
LUBEXX     |9 misura stampante Lubex primo piano (Epson DFX 5000+)

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

       77  CALLING-PROGRAM   pic x(20).
       01                    pic x.
           88 st-crt         value "C".
           88 st-graf        value "S".

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
                    if calling-program = "stdoccsv"
                       move stbolle-path-csv to path-csvInput
                       open input csvInput
                       perform until 1 = 2
                          read csvInput next 
                               at end exit perform 
                           not at end
                               unstring csvInput-riga delimited by ";"
                                   into tor-anno-bolla
                                        tor-num-bolla
                               end-unstring      
                               read tordini no lock key k-bolla
                               move tor-anno      to stbolle-anno 
                                                     save-anno
                               move tor-numero    to stb-numero-da 
                                                     stb-numero-a
                                                     save-numero
                               perform ELABORAZIONE
                               call "W$BITMAP" using wbitmap-destroy, 
                                                     BitmapSfondoHandle
                          end-read
                       end-perform
                       close csvInput
                    else
                       perform ELABORAZIONE
                       |||STATO ORDINE XXX
                       if stbolle-stampa
                          perform AGGIORNA-MASTER
                       end-if
                       |||XXX                    
                       if stbolle-piu-stampe-si
                          perform CHIUDI-FILES-STAMPANTI
                       end-if
                    end-if
                 end-if
              end-if                   
              perform CLOSE-FILES
           end-if.
           perform EXIT-PGM.

      ***---
       INIT.             
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
           CALL "C$CALLEDBY" USING CALLING-PROGRAM
           evaluate CALLING-PROGRAM
           when "stbolle-r"
           when "stbolle"                   
                set st-crt   to true
           when other

                set st-graf  to true
      *    se è grafica chiedo subito la stampante, così se l'utente
      *    annulla esco subito senza far niente
                if calling-program = "invio-sol"
                   set GeneraPdf to true
                   perform CREA-PDF
                   if settaPDF-OK
                     |Va bene lo stesso delle conferme
                      accept selprint-stampante 
                      from environment "CONFERMA_STAMPANTE_DIRETTA"
                   else
                      goback
                   end-if
                else
                   call   "selprint" using selprint-linkage
                   cancel "selprint"
                   if selprint-stampante = space
                      goback
                   end-if
                end-if
           end-evaluate.                               

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
           call "C$CALLEDBY" using LinkPgm.
           if LinkPgm = "solleciti" or GeneraPdf
              move "gordcvar" to LinkPgm
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

           accept  righe-page-feed-x from environment "RIGHE_PAGE_FEED".
           call "C$JUSTIFY"  using righe-page-feed-x, "R".
           inspect righe-page-feed-x replacing leading x"20" by x"30".
           move    righe-page-feed-x to righe-page-feed convert.
          
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
              if st-crt
                 open output lineseq
              end-if
              if tutto-ok
                 open input  clienti  destini  tvettori assorcli param
                             articoli tcaumag  tnomen tmarche timposte
                             listini  reltor   ttipocli |evaclides
                             tparamge prodener tscorte 
                             progmag
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
                             if LinkPgm = "gordcvar"
                                perform AGGIUNGI-STAMPANTE
                             else
      *                          if tor-anno-fattura = 0 and 
      *                             tor-data-fattura = 0 and
      *                             tor-num-prenot   = 0
                                   perform AGGIUNGI-STAMPANTE
      *                          end-if
                             end-if
                          end-if
                       end-if
                    end-if
                 end-if

              end-perform
           end-if.

      ***---
       AGGIUNGI-STAMPANTE.
      *****     move tor-cod-cli     to como-prm-cliente.
      *****     move tor-prg-destino to como-prm-destino.
      *****     perform TROVA-PARAMETRO.
           accept prm-stampante from environment "STAMPANTE_STBOLLE".
           if prm-stampante = spaces
              set errori to true
              display message 
              "CONFIGURARE STAMPANTE PER CLIENTE " tor-cod-cli
                        title titolo
                         icon 2
           else
              move 0 to idx-stampante
      *****        inspect prm-stampante converting
      *****                            "abcdefghijklmnopqrstuvwxyz" TO
      *****                            "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
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
                    if tor-note1        not = spaces or
                       tor-note2        not = spaces or
                       tor-note3        not = spaces or
                       tor-note4        not = spaces or
                       tor-data-note1   not = 0      or
                       tor-note-bolla-1 not = spaces or
                       tor-note-bolla-2 not = spaces

                       evaluate true
                       when st-crt
                            move NumRigheConNote        to RowsPerPage
                       when st-graf
                            move NumRigheConNote-graf   to RowsPerPage
                       end-evaluate                         
                       set esiste-note to true
                    else
                       evaluate true
                       when st-crt
                            move NumRigheSenzaNote      to RowsPerPage
                       when st-graf
                            move NumRigheSenzaNote-graf to RowsPerPage
                       end-evaluate
                       set esiste-note to false
                    end-if      
                    
                    initialize tca-caus-trasporto
                    move tor-causale to tca-codice
                    read tcaumag no lock invalid continue end-read

                    if tca-si-stampa or calling-program = "gordcvar"

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
                                            perform STAMPA-DATI
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
                             if LinkPgm = "gordcvar"
                                perform STAMPA-DATI
                             else
      *                          if tor-anno-fattura = 0 and 
      *                             tor-data-fattura = 0 and
      *                             tor-num-prenot   = 0
                                   perform STAMPA-DATI
      *                          end-if
                             end-if
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
                 display message "Nessun documento presente avente"
                                 " il criterio selezionato"
                           title titolo
                            icon 2
              end-if
           else
              if LinkPgm not = "gordcvar"
                 if st-crt
                    |Faccio scrivere solamente sul txt principale
                    move 0 to idx-stampante
                    compute n-vuote = 6 + righe-page-feed
                    perform PAGE-FEED
                    perform RESET-BUFFER-PRINTER

                    |NON faccio scrivere sul txt principale
                    set NoTXTPrincipale to true
                    perform varying idx-stampante from 1 by 1 
                              until idx-stampante > idx-tot-stampanti
                       compute n-vuote = 6 + righe-page-feed
                       perform PAGE-FEED
                       perform RESET-BUFFER-PRINTER
                    end-perform
                    set NoTXTPrincipale to false
                    display message 
                                "Inserire modulo continuo e premere OK"
                           title = titolo
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
           if RecLocked
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
              
      *****        move tor-cod-cli     to como-prm-cliente
      *****        move tor-prg-destino to como-prm-destino
      *****        perform TROVA-PARAMETRO
      *****
      *****        inspect prm-stampante converting
      *****                            "abcdefghijklmnopqrstuvwxyz" TO
      *****                            "ABCDEFGHIJKLMNOPQRSTUVWXYZ"     
             accept prm-stampante from environment "STAMPANTE_STBOLLE"

              perform varying idx-stampante from 1 by 1 
                        until idx-stampante > idx-tot-stampanti
                 if prm-stampante = el-stampante(idx-stampante)
                    exit perform
                 end-if
              end-perform
           end-if.
           move 1 to PagePerBolla.
           set trovato to true.
           evaluate true
           when st-crt
                perform STAMPA-TESTA
                perform STAMPA-CORPO
                perform STAMPA-NOTE-TOTALI
           when st-graf
                perform STAMPA-TESTA-GRAF
                perform STAMPA-CORPO-GRAF
                perform STAMPA-NOTE-TOTALI-GRAF
           end-evaluate.

      ***---
       STAMPA-TESTA.
           move tor-cod-cli to cli-codice.
           set cli-tipo-C   to true.
           read clienti     no lock invalid continue end-read.
           move cli-tipo to tcl-codice.
           read ttipocli no lock invalid continue end-read.

           if LinkPgm not = "gordcvar"
              
              evaluate idx-stampante
              when 1 
                   if not PrimaVolta1
                      move RigheVuoteIntestazione     to n-vuote
                   else
LUBEXX                evaluate tcl-serie-bolle
                      when 1 |stbolle-GDO
LUBEXX                     subtract 1 
                               from RigheVuoteIntestazioneCompuprint 
LUBEXX                       giving n-vuote
                      when 2 |stbolle-MV
                      when 3 |stbolle-AT
                           subtract 1 
                               from RigheVuoteIntestazioneEpsonDFX
LUBEXX                       giving n-vuote
LUBEXX                end-evaluate

                      set PrimaVolta1 to false
                   end-if
              when 2
                   if not PrimaVolta2
                      move RigheVuoteIntestazione     to n-vuote
                   else
LUBEXX                evaluate tcl-serie-bolle
                      when 1 |stbolle-GDO
LUBEXX                     subtract 1 
                               from RigheVuoteIntestazioneCompuprint 
LUBEXX                       giving n-vuote
                      when 2 |stbolle-MV
                      when 3 |stbolle-AT
                           subtract 1 
                               from RigheVuoteIntestazioneEpsonDFX
LUBEXX                       giving n-vuote
LUBEXX                end-evaluate

                      set PrimaVolta2 to false
                   end-if
              when 3
                   if not PrimaVolta3
                      move RigheVuoteIntestazione     to n-vuote
                   else
LUBEXX                evaluate tcl-serie-bolle
                      when 1 |stbolle-GDO
LUBEXX                     subtract 1 
                               from RigheVuoteIntestazioneCompuprint 
LUBEXX                       giving n-vuote
                      when 2 |stbolle-MV
                      when 3 |stbolle-AT
                           subtract 1 
                               from RigheVuoteIntestazioneEpsonDFX
LUBEXX                       giving n-vuote
LUBEXX                end-evaluate

                      set PrimaVolta3 to false
                   end-if
              when 4
                   if not PrimaVolta4
                      move RigheVuoteIntestazione     to n-vuote
                   else
LUBEXX                evaluate tcl-serie-bolle
                      when 1 |stbolle-GDO
LUBEXX                     subtract 1 
                               from RigheVuoteIntestazioneCompuprint 
LUBEXX                       giving n-vuote
                      when 2 |stbolle-MV
                      when 3 |stbolle-AT
                           subtract 1 
                               from RigheVuoteIntestazioneEpsonDFX
LUBEXX                       giving n-vuote
LUBEXX                end-evaluate

                      set PrimaVolta4 to false
                   end-if
              when 5
                   if not PrimaVolta5
                      move RigheVuoteIntestazione     to n-vuote
                   else
LUBEXX                evaluate tcl-serie-bolle
                      when 1 |stbolle-GDO
LUBEXX                     subtract 1 
                               from RigheVuoteIntestazioneCompuprint 
LUBEXX                       giving n-vuote
                      when 2 |stbolle-MV
                      when 3 |stbolle-AT
                           subtract 1 
                               from RigheVuoteIntestazioneEpsonDFX
LUBEXX                       giving n-vuote
LUBEXX                end-evaluate

                      set PrimaVolta5 to false
                   end-if
              end-evaluate  
170718        subtract 2 from n-vuote
 

      *****        if not PrimaVolta
      *****           move RigheVuoteIntestazione     to n-vuote
      *****        else
LUBEXX*****           evaluate tcl-serie-bolle
      *****           when 1 |stbolle-GDO
LUBEXX*****                subtract 1 
      *****                    from RigheVuoteIntestazioneCompuprint 
LUBEXX*****                  giving n-vuote
      *****           when 2 |stbolle-MV
      *****           when 3 |stbolle-AT
      *****                subtract 1 
      *****                    from RigheVuoteIntestazioneEpsonDFX
LUBEXX*****                  giving n-vuote
LUBEXX*****           end-evaluate
      *****
      *****           set PrimaVolta to false
      *****        end-if  


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
              end-if

           else
              if stampa--segue
                 perform SALTO-PAGINA-A4
              end-if
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
                move vet-indirizzo(1:40)   to st-vet-ind
                move vet-indirizzo(41:40)  to st-vet-ind2
           end-read.

           move tor-numero     to st-numord.
           call "C$JUSTIFY" using st-numord, "R".

           initialize line-riga.
           if LinkPgm not = "gordcvar"
              move I-2L to line-riga
              move X"1B5701" to line-riga
           end-if.
           perform STAMPA-RIGA.

           initialize line-riga.
           if LinkPgm = "gordcvar"
              move st-vettore               to st-vettore-gv
              move st-numord                to st-numord-gv
              move st-riga-vettore-gordcvar to line-riga
           else
              move st-riga-vettore          to line-riga
           end-if.
           perform STAMPA-RIGA.

           if LinkPgm not = "gordcvar"
              write line-riga from st-riga-vettore after 0
              write line-riga from st-riga-vettore after 0
              evaluate idx-stampante
              when 1 write line-riga1 from st-riga-vettore after 0
              when 2 write line-riga2 from st-riga-vettore after 0
              when 3 write line-riga3 from st-riga-vettore after 0
              when 4 write line-riga4 from st-riga-vettore after 0
              when 5 write line-riga5 from st-riga-vettore after 0
              end-evaluate
              evaluate idx-stampante
              when 1 write line-riga1 from st-riga-vettore after 0
              when 2 write line-riga2 from st-riga-vettore after 0
              when 3 write line-riga3 from st-riga-vettore after 0
              when 4 write line-riga4 from st-riga-vettore after 0
              when 5 write line-riga5 from st-riga-vettore after 0
              end-evaluate
           end-if.

           initialize line-riga.
           if LinkPgm not = "gordcvar"
              move D-2L      to line-riga
              move x"1B5700" to line-riga
           end-if.
           perform STAMPA-RIGA.

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

           initialize line-riga.
           move st-testa to line-riga.
           perform STAMPA-RIGA.

      ***---
       NUMERI-BOLLA-COLLEGATI.    
           if LinkPgm not = "gordcvar"
              move all "#" to st-elimina
              move st-x    to line-riga
              write line-riga after 1
              
              evaluate idx-stampante
              when 1 write line-riga1 from line-riga after 1
              when 2 write line-riga2 from line-riga after 1
              when 3 write line-riga3 from line-riga after 1
              when 4 write line-riga4 from line-riga after 1
              when 5 write line-riga5 from line-riga after 1
              end-evaluate

              move all "=" to st-elimina
              move st-x    to line-riga
              write line-riga after 0
              
              evaluate idx-stampante
              when 1 write line-riga1 from line-riga after 0
              when 2 write line-riga2 from line-riga after 0
              when 3 write line-riga3 from line-riga after 0
              when 4 write line-riga4 from line-riga after 0
              when 5 write line-riga5 from line-riga after 0
              end-evaluate

              move all "X" to st-elimina
              move st-x    to line-riga
              write line-riga after 0
              
              evaluate idx-stampante
              when 1 write line-riga1 from line-riga after 0
              when 2 write line-riga2 from line-riga after 0
              when 3 write line-riga3 from line-riga after 0
              when 4 write line-riga4 from line-riga after 0
              when 5 write line-riga5 from line-riga after 0
              end-evaluate
           else
              move st-x to line-riga
              perform STAMPA-RIGA
           end-if.

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
           if LinkPgm not = "gordcvar"
              move 1 to n-vuote
           else
              move 1 to n-vuote
           end-if.
           perform RIGHE-VUOTE.

           move tor-anno   to ror-anno of rordini.
           move tor-numero to ror-num-ordine of rordini.
           move low-value  to ror-num-riga of rordini
                              ror-chiave-ordine OF rordini

           start rordini key is >= ror-k-stbolle of rordini
                 invalid set errori to true
           end-start.

           initialize old-ror-chiave-ordine-testa
           move 0 to tot-colli
                     tot-peso-utf
                     tot-peso-non-utf
                     tot-peso-tot
                     WrittenRows.
                                  
           if LinkPgm not = "gordcvar"
              move I-17 to line-riga
              write line-riga after 0
              
              evaluate idx-stampante
              when 1 write line-riga1 from line-riga after 0
              when 2 write line-riga2 from line-riga after 0
              when 3 write line-riga3 from line-riga after 0
              when 4 write line-riga4 from line-riga after 0
              when 5 write line-riga5 from line-riga after 0
              end-evaluate

           else
              move  all"-" to line-riga(1:130)
              perform STAMPA-RIGA
           end-if.

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

              perform SALTO-PAGINA-MODULO-CONTINUO

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
                 move "OMAGGIO"  to st-codart-cli
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
              compute tot-peso-utf =
                      tot-peso-utf + 
                    ( ror-peso-utf of rordini * ror-qta of rordini )
              compute tot-peso-non-utf =
                      tot-peso-non-utf + 
                    ( ror-peso-non-utf of rordini * ror-qta of rordini )
              compute tot-peso-tot =
                      tot-peso-tot + 
                   (( ror-peso-utf of rordini + 
                      ror-peso-non-utf of rordini ) * 
                      ror-qta of rordini )
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
              if tcl-serie-bolle = 2
                 move st-cod-art    to st-cod-art-s2
                 move st-cod-dog    to st-cod-dog-s2
                 move st-colli      to st-colli-s2
                 move st-imb        to st-imb-s2
                 move st-des-art    to st-des-art-s2
                 move st-codart-cli to st-codart-cli-s2
                 move st-peso-utf   to st-peso-utf-s2
                 move st-udm        to st-udm-s2
                 move st-qta        to st-qta-s2
                 move st-dec-qta    to st-dec-qta-s2
                 move struttura-stampa-serie2 to line-riga
              else
                 move struttura-stampa to line-riga
              end-if
              |23/05/2012
              perform STAMPA-RIGA
              add 1 to WrittenRows
 
OMAGGI        if ror-qta-omaggi of rordini not = 0
OMAGGI           perform SALTO-PAGINA-MODULO-CONTINUO
OMAGGI           move ror-qta-omaggi of rordini to st-qta-oma
OMAGGI           move st-riga-omaggi to line-riga
OMAGGI           perform STAMPA-RIGA
OMAGGI           add 1 to WrittenRows
OMAGGI        end-if

              if stbolle-stampa perform AGGIORNA-PROGMAG end-if
           end-perform.

      ***---
       AGGIORNA-PROGMAG.
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
           if LinkPgm not = "gordcvar"
              move D-17 to line-riga
              write line-riga after 0
              
              evaluate idx-stampante
              when 1 write line-riga1 from line-riga after 0
              when 2 write line-riga2 from line-riga after 0
              when 3 write line-riga3 from line-riga after 0
              when 4 write line-riga4 from line-riga after 0
              when 5 write line-riga5 from line-riga after 0
              end-evaluate

           end-if.

           if LinkPgm = "gordcvar"
              move all "-" to line-riga(1:130)
           else
              move all "-" to line-riga(1:77)
           end-if.
           perform STAMPA-RIGA.

           move PagePerBolla  to st-num-page.

           if LinkPgm = "gordcvar"
              move st-num-page            to st-num-page-gv
              move st-riga-segue-gordcvar to line-riga
              perform STAMPA-RIGA              
           else
              move st-riga-segue to line-riga
              perform STAMPA-RIGA
              if esiste-note move 26 to n-vuote
              else           move 24 to n-vuote
              end-if
              add n-vuote-from-divisorio-master to n-vuote
              perform RIGHE-VUOTE
           end-if.

      ***---
       STAMPA-NOTE-TOTALI.
           subtract WrittenRows from RowsPerPage giving n-vuote.
           perform RIGHE-VUOTE.
              
           move 0 to st-tot-peso-utf st-tot-peso-non-utf st-tot-peso-tot
           if LinkPgm not = "gordcvar"
              move D-17 to line-riga
              write line-riga after 0
              
              evaluate idx-stampante
              when 1 write line-riga1 from line-riga after 0
              when 2 write line-riga2 from line-riga after 0
              when 3 write line-riga3 from line-riga after 0
              when 4 write line-riga4 from line-riga after 0
              when 5 write line-riga5 from line-riga after 0
              end-evaluate

           end-if.

           initialize line-riga.
           move PagePerBolla to PagePerBollaEdit.
           string "*Tot. Pag. "    delimited size
                  PagePerBollaEdit delimited size
                  " - FINE*"       delimited size
                  into line-riga
           end-string.

           if LinkPgm = "gordcvar"
              move 110 to limite
           else
              move 55 to limite
           end-if.
           move line-riga(1:21) to line-riga(limite:21)
           move  all"-" to line-riga(1:limite - 1)

           perform STAMPA-RIGA.

           if esiste-note
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

              initialize line-riga
              move st-riga-piede-1 to line-riga
              perform STAMPA-RIGA
           end-if.             

           move  tot-colli to st-tot-colli.
           move  tor-note2 to st-note-2.
           initialize line-riga.
           move st-riga-piede-2 to line-riga.
           perform STAMPA-RIGA.   

           if esiste-note
              move  tor-note3 to st-note-3
              initialize line-riga
              move st-riga-piede-3 to line-riga
              perform STAMPA-RIGA

              move  tor-note4 to st-note-4
              initialize line-riga
              move st-riga-piede-4 to line-riga
              perform STAMPA-RIGA 

              move I-17 to line-riga
              write line-riga after 0 
              evaluate idx-stampante
              when 1 write line-riga1 from line-riga after 0
              when 2 write line-riga2 from line-riga after 0
              when 3 write line-riga3 from line-riga after 0
              when 4 write line-riga4 from line-riga after 0
              when 5 write line-riga5 from line-riga after 0
              end-evaluate  

              move  tor-note-bolla-1 to st-note-5
              initialize line-riga
              move st-riga-piede-5 to line-riga
              perform STAMPA-RIGA

              move  tor-note-bolla-2 to st-note-6
              initialize line-riga
              move st-riga-piede-6 to line-riga
              perform STAMPA-RIGA
              
              move D-17 to line-riga
              write line-riga after 0 
              evaluate idx-stampante
              when 1 write line-riga1 from line-riga after 0
              when 2 write line-riga2 from line-riga after 0
              when 3 write line-riga3 from line-riga after 0
              when 4 write line-riga4 from line-riga after 0
              when 5 write line-riga5 from line-riga after 0
              end-evaluate
           end-if.
                                    
           if LinkPgm not = "gordcvar"
              if esiste-note
                 move 1 to n-vuote
              else
                 move 2 to n-vuote
              end-if
              perform RIGHE-VUOTE
           end-if.

           move spaces to dicitura-vett.
           if tor-vettore not = 0
              set riga-vettore to true
              if LinkPgm = "gordcvar"
                 move "Vettore: " to dicitura-vett
              end-if
           else
              set riga-vettore to false
           end-if.

           write line-riga from spaces.
           evaluate idx-stampante
           when 1 write line-riga1 from spaces
           when 2 write line-riga2 from spaces
           when 3 write line-riga3 from spaces
           when 4 write line-riga4 from spaces
           when 5 write line-riga5 from spaces
           end-evaluate.

           initialize line-riga.
           move st-riga-vett-x to line-riga.
           perform STAMPA-RIGA.

           move  tot-peso-utf to st-tot-peso-utf.
           initialize line-riga.
           if LinkPgm = "gordcvar"
              move st-tot-peso-utf      to st-tot-peso-utf-gv
              move st-riga-utf-gordcvar to line-riga
           else
              move st-riga-utf to line-riga
           end-if.
           perform STAMPA-RIGA.

           move  tot-peso-non-utf to st-tot-peso-non-utf.
           initialize line-riga.
           if LinkPgm = "gordcvar"
              move st-tot-peso-non-utf      to st-tot-peso-non-utf-gv
              move st-riga-non-utf-gordcvar to line-riga
           else
              move st-riga-non-utf to line-riga
           end-if.
           perform STAMPA-RIGA.

           move  tot-peso-tot to st-tot-peso-tot.
           initialize line-riga.
           if LinkPgm = "gordcvar"
              move st-tot-peso-tot      to st-tot-peso-tot-gv
              move st-riga-tot-gordcvar to line-riga
           else
              move st-riga-tot to line-riga
           end-if.
           perform STAMPA-RIGA.

           if LinkPgm = "gordcvar" move 1 to n-vuote
           else                    move 4 to n-vuote
           end-if.
           perform RIGHE-VUOTE.

           if tor-vettore not = 0 and LinkPgm = "gordcvar"
              move "VETTORE: DITTA, RESIDENZA O DOMICILIO" to line-riga
              perform STAMPA-RIGA
           end-if.

           if LinkPgm not = "gordcvar"
              move I-17 to line-riga
              write line-riga after 0 
              evaluate idx-stampante
              when 1 write line-riga1 from line-riga after 0
              when 2 write line-riga2 from line-riga after 0
              when 3 write line-riga3 from line-riga after 0
              when 4 write line-riga4 from line-riga after 0
              when 5 write line-riga5 from line-riga after 0
              end-evaluate
           end-if.

           initialize line-riga.
           move st-vettore-estesa-1 to line-riga.
           perform STAMPA-RIGA.      

           initialize line-riga.
           move st-vettore-estesa-2 to line-riga.
           perform STAMPA-RIGA.

           initialize line-riga.
           move st-vettore-estesa-2b to line-riga.
           perform STAMPA-RIGA.

           if LinkPgm not = "gordcvar"
              move D-17 to line-riga
              write line-riga after 0
              evaluate idx-stampante
              when 1 write line-riga1 from line-riga after 0
              when 2 write line-riga2 from line-riga after 0
              when 3 write line-riga3 from line-riga after 0
              when 4 write line-riga4 from line-riga after 0
              when 5 write line-riga5 from line-riga after 0
              end-evaluate        
              move -2 to n-vuote
170718        add 2   to n-vuote
              perform PAGE-FEED
           end-if.

      ***---
       PAGE-FEED.
           |Faccio scarrellare fino alla prima riga 
           |della pagina successiva nel modulo
           add 10 to n-vuote.
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
           perform STAMPA-DATI.

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
                   perform STAMPA-DATI
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
       SALTO-PAGINA-MODULO-CONTINUO.
           if WrittenRows = RowsPerPage
              perform STAMPA-SEGUE  

170718        move 2 to n-vuote
170718        perform RIGHE-VUOTE

              perform STAMPA-TESTA

              if LinkPgm not = "gordcvar"
                 move 1 to n-vuote 

                 perform RIGHE-VUOTE
                 move I-17 to line-riga
                 write line-riga after 0
              
                 evaluate idx-stampante
                 when 1 write line-riga1 from line-riga after 0
                 when 2 write line-riga2 from line-riga after 0
                 when 3 write line-riga3 from line-riga after 0
                 when 4 write line-riga4 from line-riga after 0
                 when 5 write line-riga5 from line-riga after 0
                 end-evaluate

              else
                 move 1 to n-vuote
                 perform RIGHE-VUOTE
                 move  all"-" to line-riga(1:130)
                 perform STAMPA-RIGA
              end-if

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
       SALTO-PAGINA-A4.
           if LinkPgm = "gordcvar"
              write line-riga from spaces after page
              write line-riga from x"09" after 1
           end-if.

      ***---
       RIGHE-VUOTE.
           evaluate idx-stampante
           when 1 
                if LinkPgm not = "gordcvar" and not PrimaVolta1
                   perform RESET-BUFFER-PRINTER
                end-if           
           when 2
                if LinkPgm not = "gordcvar" and not PrimaVolta2
                   perform RESET-BUFFER-PRINTER
                end-if
           when 3
                if LinkPgm not = "gordcvar" and not PrimaVolta3
                   perform RESET-BUFFER-PRINTER
                end-if
           when 4
                if LinkPgm not = "gordcvar" and not PrimaVolta4
                   perform RESET-BUFFER-PRINTER
                end-if
           when 5
                if LinkPgm not = "gordcvar" and not PrimaVolta5
                   perform RESET-BUFFER-PRINTER
                end-if
           end-evaluate.

      *****     if LinkPgm not = "gordcvar" and not PrimaVolta
      *****        perform RESET-BUFFER-PRINTER
      *****     end-if.

           perform n-vuote times
              if not NoTXTPrincipale
                 write line-riga from spaces after 1
              end-if
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
           write line-riga.
           evaluate idx-stampante
           when 1 write line-riga1 from line-riga
           when 2 write line-riga2 from line-riga
           when 3 write line-riga3 from line-riga
           when 4 write line-riga4 from line-riga
           when 5 write line-riga5 from line-riga
           end-evaluate.

      ***---
       RESET-BUFFER-PRINTER.
           |Inizializzazione del buffer stampante
           move ESC-ESC to line-riga.
           if not NoTXTPrincipale
              write line-riga after 0
           end-if.
           evaluate idx-stampante
           when 1 write line-riga1 from line-riga after 0
           when 2 write line-riga2 from line-riga after 0
           when 3 write line-riga3 from line-riga after 0
           when 4 write line-riga4 from line-riga after 0
           when 5 write line-riga5 from line-riga after 0
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
                 tparamge prodener tpromo  rpromo tscorte |evaclides.

           if st-crt
              close lineseq
           else
              perform CHIUDI-STAMPA-GRAF
              if GeneraPdf
                 if settaPDF-OK
                    perform ASPETTA-PDF
                 end-if           
              end-if
           end-if.

      ***---
       EXIT-PGM.
           move wstampa to stbolle-path.
           if idx-original > 0
              evaluate true
              when st-crt
                   perform ST-DIVISIONE-CRT
              when st-graf
                   display message box "divisione ordini > 500 grafico"
              end-evaluate
           end-if.   

           if ControlloCausali
              call   "evaomag" using stbolle-anno eva-anno eva-da eva-a
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
      *****        move 1 to n-vuote-from-divisorio-master
              perform SALTO-PAGINA-MODULO-CONTINUO
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

           initialize line-riga.
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
       PARAGRAFO-COPY.
           copy "aggiorna-stato-master.cpy".
           copy "stbolle-p-graf.cpy".
           copy "direziona-impegnato-common.cpy".
           copy "trova-su-assorcli.cpy".
           copy "trova-su-listini.cpy".
           copy "trova-parametro.cpy".
