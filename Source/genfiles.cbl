       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      genfiles.
       SPECIAL-NAMES. decimal-point is comma.
      ******************************************************************

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.     
      *    Files SSI      
           copy "ABI.sl".
           copy "ART.sl".
           copy "CLI.sl".
           copy "DES.sl".
           copy "DOCCN.sl".
           copy "DOCDI.sl".
           copy "DOCES.sl".
           copy "FRN.sl".
           copy "TBLCO.sl".
           copy "TBLDO.sl".
           copy "TBLNA.sl".
           copy "TBLCA.sl".
           copy "TBLAG.sl".
           copy "TBLTR.sl".
           copy "TBLPC.sl". 
           copy "TBLVA.sl".
           copy "TBLME.sl".
           copy "TBLCS.sl".
           copy "FPGRUPPICS.sl".
           copy "PAT.sl".
           copy "PAS.sl".
           copy "PAR.sl".
           copy "PNT.sl".
           copy "PNR.sl".
           copy "PNI.sl".
      *****     copy "MAZ.sl".
      *****     copy "CLZ.sl".
      *****     copy "FRZ.sl".

      *    Files GESLUX
           copy "GDVR.sl".
           copy "GDVT.sl".
           copy "Prog.sl".                                 
           copy "USER.sl".
           copy "user-pgm.sl".
           copy "tregioni.sl".
           copy "tgrupgdo.sl".
           copy "tnazioni.sl".
           copy "tcla1art.sl".
           copy "tudm.sl".
           copy "tmagaz.sl".
           copy "tnomen.sl".
           copy "timballi.sl".
           copy "tvettori.sl".
           copy "tprov.sl".
           copy "ttipocli.sl".
           copy "agenti.sl". 
           copy "clienti.sl". 
           copy "destini.sl". 
           copy "destinif.sl". 
           copy "recapiti.sl".  
           copy "tsetmerc.sl". 
           copy "note.sl".
           copy "tmarche.sl".
           copy "tcodpag.sl".
           copy "tivaese.sl".
           copy "timposte.sl".
           copy "articoli.sl".
           copy "assorcli.sl".
           copy "progmag.sl".
           copy "tordini.sl".
           copy "rordini.sl".
           copy "eordini.sl".
      *****     copy "int-tordini.sl".
      *****     copy "int-rordini.sl".
      *****     copy "btordini.sl".
      *****     copy "brordini.sl".
           copy "tnotacr.sl".
           copy "rnotacr.sl".
           copy "btnotacr.sl".
           copy "brnotacr.sl".
           copy "tcontat.sl".
           copy "tcaumag.sl".
           copy "tmovmag.sl".
           copy "rmovmag.sl".
           copy "timbalqta.sl".
           copy "tparamge.sl".
           copy "tparamge2.sl".
           copy "distinteb.sl".
           copy "tcaudisb.sl".
           copy "statraff.sl".
           copy "statsett.sl".
           copy "giormag.sl".
           copy "tgiormag.sl".
           copy "ordfor.sl".
           copy "ordfor2.sl".
           copy "ordfor-old.sl".
           copy "movutf.sl".
           copy "anautf.sl".
           copy "tmovtrat.sl".
           copy "lisagente.sl".
           copy "provvig.sl".
           copy "tarifvet.sl".
           copy "trasporti.sl".
           copy "statmese.sl".
           copy "G2.sl".
           copy "tconvanno.sl".
           copy "lockfile.sl".
           copy "lockname.sl".
           copy "logfile.sl".
           copy "tpiombo.sl".
           copy "vettel.sl".
           copy "tescons.sl".
           copy "tesconsvet.sl".
           copy "tscorte.sl".
           copy "tlistini.sl".
           copy "rlistini.sl".
           copy "nforn.sl".
           copy "nforn-dest.sl".
           copy "ttipoavv.sl".
           copy "useravv.sl".
           copy "tordforn.sl".
           copy "rordforn.sl".
           copy "sordforn.sl".
           copy "catart.sl".
           copy "promoeva.sl".
           copy "tparameva.sl".
      *****     copy "evaclides.sl".
           copy "pgmexe.sl".

LABLAB     copy "tagli.sl".
LABLAB     copy "listini.sl".
LABLAB     copy "tpromo.sl".
LABLAB     copy "rpromo.sl".
LABLAB     copy "usr-tel.sl".
LABLAB     copy "locali.sl".
LABLAB     copy "blister.sl".

           copy "progmagric.sl".
           copy "contestazioni.sl".
           copy "note-cont.sl".
           copy "reltor.sl".
           copy "coperfab.sl".
           copy "teva.sl".
           copy "reva.sl".
           copy "notef.sl".
           copy "art-ordforn.sl".
           copy "qta-vend.sl".
           copy "mtordini.sl".
           copy "mrordini.sl".
           copy "impforn.sl". 
           copy "sitfin.sl".
           copy "nlistini.sl".
           copy "art-exp-shi.sl".
           copy "EDI-sl.def".

           copy "param.sl".
           copy "multigest.sl".
           copy "paramshi.sl".
           copy "paramget.sl".
           copy "cli-prg.sl".
           copy "contab.sl".
           copy "pagbloc.sl".
           copy "tgruppi.sl".
           copy "tsetinvio.sl".
           copy "batnott.sl".
           copy "lineseq.sl".
           copy "comuni.sl".
           copy "ttipodoc.sl".     
           copy "stato-invio.sl".
           copy "qta-pordini.sl".
           copy "paramfel.sl".
           copy "tnumordf.sl".
           copy "grade.sl".
           copy "battsost.sl".

           copy "macrobatch.sl".
           copy "hleb.sl".
           copy "anacap.sl".          
                              
           copy "lock-div.sl".
           copy "log4mas.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.     
      *    Files SSI     
           copy "ABI.fd".
           copy "ART.fd".
           copy "CLI.fd".
           copy "DES.fd".
           copy "DOCCN.fd".
           copy "DOCDI.fd".
           copy "DOCES.fd".
           copy "FRN.fd".
           copy "TBLCO.fd".
           copy "TBLDO.fd".
           copy "TBLNA.fd".
           copy "TBLCA.fd".
           copy "TBLAG.fd".
           copy "TBLTR.fd".
           copy "TBLPC.fd".
           copy "TBLVA.fd".    
           copy "TBLME.fd".
           copy "TBLCS.fd".
           copy "FPGRUPPICS.fd".
           copy "PAT.fd".
           copy "PAS.fd".
           copy "PAR.fd".        
           copy "PNT.fd".
           copy "PNR.fd".
           copy "PNI.fd".
      *****     copy "MAZ.fd".
      *****     copy "CLZ.fd".
      *****     copy "FRZ.fd".

      *    Files GESLUX    
           copy "GDVR.fd".
           copy "GDVT.fd".
           copy "Prog.fd".                                 
           copy "USER.fd".    
           copy "user-pgm.fd".
           copy "tregioni.fd".
           copy "tgrupgdo.fd".
           copy "tnazioni.fd".
           copy "tcla1art.fd".
           copy "tudm.fd".
           copy "tmagaz.fd".
           copy "tnomen.fd".
           copy "timballi.fd".
           copy "tvettori.fd".
           copy "tprov.fd".
           copy "ttipocli.fd".
           copy "agenti.fd".  
           copy "clienti.fd". 
           copy "destini.fd".   
           copy "destinif.fd".   
           copy "recapiti.fd".  
           copy "tsetmerc.fd". 
           copy "note.fd".
           copy "tmarche.fd".
           copy "tcodpag.fd".
           copy "tivaese.fd".
           copy "timposte.fd".
           copy "articoli.fd".
           copy "assorcli.fd".
           copy "progmag.fd".
           copy "tordini.fd".
           copy "rordini.fd".
           copy "eordini.fd".
      *****     copy "int-tordini.fd".
      *****     copy "int-rordini.fd".
      *****     copy "btordini.fd".
      *****     copy "brordini.fd".
           copy "tnotacr.fd".
           copy "rnotacr.fd". 
           copy "btnotacr.fd".
           copy "brnotacr.fd".
           copy "tcontat.fd".
           copy "tcaumag.fd".
           copy "tmovmag.fd".
           copy "rmovmag.fd".
           copy "timbalqta.fd".
           copy "tparamge.fd".
           copy "tparamge2.fd".
           copy "distinteb.fd".
           copy "tcaudisb.fd".
           copy "statraff.fd".
           copy "statsett.fd".
           copy "giormag.fd".
           copy "tgiormag.fd".
           copy "ordfor.fd".
           copy "ordfor2.fd".
           copy "ordfor-old.fd".
           copy "movutf.fd".
           copy "anautf.fd".
           copy "tmovtrat.fd".           
           copy "lisagente.fd".
           copy "provvig.fd". 
           copy "tarifvet.fd". 
           copy "trasporti.fd".
           copy "statmese.fd".
           copy "G2.fd".
           copy "tconvanno.fd".
           copy "lockfile.fd". 
           copy "lockname.fd".
           copy "logfile.fd".
           copy "tpiombo.fd".
           copy "vettel.fd".
           copy "tescons.fd".
           copy "tesconsvet.fd".
           copy "tscorte.fd".
           copy "tlistini.fd".
           copy "rlistini.fd".
           copy "nforn.fd".
           copy "nforn-dest.fd".
           copy "ttipoavv.fd".
           copy "useravv.fd". 
           copy "tordforn.fd".
           copy "rordforn.fd".
           copy "sordforn.fd".
           copy "catart.fd".
           copy "promoeva.fd".
           copy "tparameva.fd".
      *****     copy "evaclides.fd".
           copy "pgmexe.fd".

LABLAB     copy "tagli.fd".
LABLAB     copy "listini.fd".
LABLAB     copy "tpromo.fd".
LABLAB     copy "rpromo.fd".
LABLAB     copy "usr-tel.fd".
LABLAB     copy "locali.fd". 
LABLAB     copy "blister.fd".

           copy "progmagric.fd".
           copy "contestazioni.fd".
           copy "note-cont.fd".   
           copy "reltor.fd".
           copy "coperfab.fd".
           copy "teva.fd".
           copy "reva.fd".
           copy "notef.fd".
           copy "art-ordforn.fd".
           copy "qta-vend.fd".
           copy "mtordini.fd".
           copy "mrordini.fd".
           copy "impforn.fd".
           copy "sitfin.fd".
           copy "nlistini.fd".
           copy "art-exp-shi.fd".

           copy "EDI-fd.def".
           copy "param.fd".
           copy "multigest.fd".
           copy "paramshi.fd".
           copy "paramget.fd".
           copy "cli-prg.fd".
           copy "contab.fd".
           copy "pagbloc.fd".
           copy "tgruppi.fd".  
           copy "tsetinvio.fd".  
           copy "batnott.fd".
           copy "lineseq.fd".  
           copy "comuni.fd".
           copy "ttipodoc.fd".   
           copy "stato-invio.fd".
           copy "qta-pordini.fd".   
           copy "paramfel.fd".
           copy "tnumordf.fd".
           copy "grade.fd".  
           copy "battsost.fd".
           copy "macrobatch.fd".  
           copy "hleb.fd".              
           copy "anacap.fd".  
           copy "lock-div.fd".

           copy "log4mas.fd".

       WORKING-STORAGE SECTION.
           COPY "acucobol.def".

      * Status Files SSI
       77  status-ABI        pic x(2).
       77  status-ART        pic x(2).
       77  status-CLI        pic x(2).
       77  status-DES        pic x(2).
       77  status-DOCCN      pic x(2).
       77  status-DOCDI      pic x(2).
       77  status-DOCES      pic x(2).
       77  status-GDVR       pic x(2).
       77  status-GDVT       pic x(2).
       77  status-FRN        pic x(2).
       77  status-TBLCO      pic x(2).
       77  status-TBLDO      pic x(2).
       77  status-TBLNA      pic x(2).
       77  status-TBLAG      pic x(2).
       77  status-TBLCA      pic x(2).
       77  status-TBLTR      pic x(2).
       77  status-TBLPC      pic x(2).
       77  status-TBLVA      pic x(2).
       77  status-TBLME      pic x(2).
       77  status-TBLCS      pic x(2).
       77  status-FPGRUPPICS pic x(2).
       77  status-PAS        pic x(2).
       77  status-PAT        pic x(2).
       77  status-PAR        pic x(2).
       77  status-PNT        pic x(2).
       77  status-PNR        pic x(2).
       77  status-PNI        pic x(2).
      ***** 77  status-MAZ        pic x(2).
      ***** 77  status-CLZ        pic x(2).

      * Status Files GESLUX  
       77  stat-prog         PIC X(2).
       77  status-user       PIC X(2).
       77  status-user-pgm   PIC X(2).
       77  status-tregioni   PIC X(2).
       77  status-tgrupgdo   PIC X(2).
       77  status-tnazioni   PIC X(2).
       77  status-tcla1art   PIC X(2).
       77  status-tudm       pic X(2).
       77  status-tmagaz     pic X(2).
       77  status-tnomen     pic X(2).
       77  status-timballi   pic X(2).
       77  status-tvettori   pic X(2).
       77  status-tprov      pic X(2).
       77  status-ttipocli   pic X(2).
       77  status-agenti     pic X(2).
       77  status-clienti    pic X(2).
       77  status-destini    pic X(2).
       77  status-destinif   pic X(2).
       77  status-recapiti   pic X(2).
       77  status-tsetmerc   pic X(2).
       77  status-note       pic X(2).
       77  status-tmarche    pic X(2).
       77  status-tcodpag    pic X(2).
       77  status-tivaese    pic X(2).
       77  status-timposte   pic X(2).
       77  status-articoli   pic X(2).
       77  status-assorcli   pic X(2).
       77  status-progmag    pic X(2).
       77  status-tordini    pic X(2).
       77  status-rordini    pic X(2).
       77  status-eordini    pic X(2).
      ***** 77  status-int-tordini    pic X(2).
      ***** 77  status-int-rordini    pic X(2).
      ***** 77  status-btordini   pic x(2).
      ***** 77  status-brordini   pic x(2).
       77  status-tnotacr    pic X(2).
       77  status-rnotacr    pic X(2).
       77  status-btnotacr   pic X(2).
       77  status-brnotacr   pic X(2).
       77  status-tcontat    pic x(2).
       77  status-tcaumag    pic x(2).
       77  status-tmovmag    pic x(2).
       77  status-rmovmag    pic x(2).
       77  status-timbalqta  pic x(2).
       77  status-tparamge   pic x(2).
       77  status-tparamge2  pic x(2).
       77  status-distinteb  pic x(2).
       77  status-tcaudisb   pic x(2).
       77  status-statraff   pic x(2).
       77  status-statsett   pic x(2).  
       77  status-giormag    pic x(2).
       77  status-tgiormag   pic x(2).
       77  status-ordfor     pic x(2).
       77  status-ordfor2    pic x(2).
       77  status-ordfor-old  pic x(2).
       77  status-movutf     pic x(2).
       77  status-anautf     pic x(2).
       77  status-tmovtrat   pic x(2).
       77  status-lisagente  pic x(2).
       77  status-provvig    pic x(2).
       77  status-tarifvet   pic x(2).
       77  status-trasporti  pic x(2).
       77  status-statmese   pic x(2).
       77  status-G2         pic x(2).
       77  status-tconvanno  pic x(2).
       77  status-lockfile   pic x(2).
       77  status-lockname   pic x(2).
       77  status-logfile    pic x(2).
       77  status-tpiombo    pic x(2).
       77  status-vettel     pic x(2).
       77  status-tescons    pic x(2).
       77  status-tesconsvet pic x(2).
       77  status-tscorte    pic x(2).
       77  status-tlistini   pic x(2).
       77  status-rlistini   pic x(2).
       77  status-nforn      pic x(2).
       77  status-nforn-dest pic x(2).
       77  status-ttipoavv   pic x(2).
       77  status-useravv    pic x(2).
       77  status-tordforn   pic x(2).
       77  status-rordforn   pic x(2).
       77  status-sordforn   pic x(2).
       77  status-catart     pic x(2).
       77  status-promoeva   pic x(2).
       77  status-tparameva  pic x(2).
      ***** 77  status-evaclides  pic x(2).
       77  status-pgmexe     pic x(2).
       77  status-param      pic x(2).

       77  status-lineseq    pic x(2).
       77  wstampa           pic x(256).

       77  status-tagli      pic x(2).
       77  status-listini    pic x(2).
       77  status-tpromo     pic x(2).
       77  status-rpromo     pic x(2).
       77  status-usr-tel    pic x(2).
       77  status-locali     pic x(2).
       77  status-blister    pic x(2).

       77  status-progmagric    pic x(2).
       77  status-contestazioni pic x(2).
       77  status-note-cont     pic x(2).
       77  status-reltor        pic x(2).
       77  status-coperfab      pic x(2).
       77  status-teva          pic x(2).
       77  status-reva          pic x(2).
       77  status-art-ordforn   pic x(2).
       77  status-notef         pic x(2).
       77  status-qta-vend      pic x(2).
       77  status-mtordini      pic x(2).
       77  status-mrordini      pic x(2).
       77  status-impforn       pic x(2).
       77  status-sitfin        pic x(2).
       77  status-nlistini      pic x(2).
       77  status-art-exp-shi   pic x(2).
       77  status-multigest     pic x(2).
       77  status-paramshi      pic x(2).
       77  status-paramget      pic x(2).
       77  status-cli-prg       pic x(2).
       77  status-contab        pic x(2).
       77  status-pagbloc       pic x(2).
       77  status-tgruppi       pic x(2).
       77  status-tsetinvio     pic x(2).
       77  status-batnott       pic x(2).
       77  status-comuni        pic x(2).
       77  status-ttipodoc      pic x(2).
       77  status-stato-invio   pic x(2).
       77  status-qta-pordini   pic x(2).
       77  status-paramfel      pic x(2).
       77  status-tnumordf      pic x(2).
       77  status-grade         pic x(2).
       77  status-battsost      pic x(2).
       77  status-macrobatch    pic x(2).
       77  status-hleb          pic x(2).
       77  status-anacap        pic x(2).
       77  status-lock-div      pic x(2).
       77  status-log4mas       pic x(2).

           copy "EDI-status.def".

       78  titolo            value "Generazione files".
                         
       77  nn pic 9(10) value 0.

       LINKAGE SECTION.
       77  link-status       pic s9.

      ******************************************************************
       PROCEDURE DIVISION USING link-status.
           COPY "DECLXER1".
                     |SSI
                     ABI
                     ART
                     CLI
                     DES
                     DOCCN
                     DOCDI
                     DOCES
                     FRN
                     TBLCO
                     TBLDO
                     TBLNA
                     TBLAG
                     TBLCA
                     TBLTR
                     TBLPC
                     TBLVA
                     TBLME
                     TBLCS
                     FPGRUPPICS
                     PAT
                     PAS
                     PAR
                     PNT
                     PNR
                     PNI     
      *****               MAZ
      *****               CLZ
                     .      
      ***---
       GDVT-ERR SECTION.
           use after error procedure on GDVT.
           evaluate status-GDVT
           when "35" continue
           when "39"
                display message "File [GDVT] Mismatch size!"
                           title titolo
                            icon 3
                
           when "98"
                display message "[GDVT] Indexed file corrupt!"
                           title titolo
                            icon 3
                
           end-evaluate.    

      ***---
       GDVR-ERR SECTION.
           use after error procedure on GDVR.
           evaluate status-GDVR
           when "35" continue
           when "39"
                display message "File [GDVR] Mismatch size!"
                           title titolo
                            icon 3
                
           when "98"
                display message "[GDVR] Indexed file corrupt!"
                           title titolo
                            icon 3
                
           end-evaluate.    

      ***---
       PROG-ERR SECTION.
           use after error procedure on Prog.
           evaluate stat-Prog
           when "35" continue
           when "39"
                display message "File [PROG] Mismatch size!"
                           title titolo
                            icon 3
                
           when "98"
                display message "[PROG] Indexed file corrupt!"
                           title titolo
                            icon 3
                
           end-evaluate.
 
      ***---
       USER-ERR SECTION.
           use after error procedure on USER.
           evaluate status-USER
           when "35" continue
           when "39"
                display message "File [USER] Mismatch size!"
                           title titolo
                            icon 3
                
           when "98"
                display message "[USER] Indexed file corrupt!"
                           title titolo
                            icon 3
                
           end-evaluate.
 
      ***---
       NOTEF-ERR SECTION.
           use after error procedure on notef.
           evaluate status-NOTEF
           when "35" continue
           when "39"
                display message "File [NOTEF] Mismatch size!"
                           title titolo
                            icon 3
                
           when "98"
                display message "[NOTEF] Indexed file corrupt!"
                           title titolo
                            icon 3
                
           end-evaluate.
 
      ***---
       USER-PGM-ERR SECTION.
           use after error procedure on user-pgm.
           evaluate status-user-pgm
           when "35" continue
           when "39"
                display message "File [USER-PGM] Mismatch size!"
                           title titolo
                            icon 3
                
           when "98"
                display message "[USER-PGM] Indexed file corrupt!"
                           title titolo
                            icon 3
                
           end-evaluate.
 
      ***---
       TREGIONI-ERR SECTION.
           use after error procedure on tregioni.
           evaluate status-tregioni
           when "35" continue
           when "39"
                display message "File [TREGIONI] Mismatch size!"
                           title titolo
                            icon 3
                
           when "98"
                display message "[TREGIONI] Indexed file corrupt!"
                           title titolo
                            icon 3
                
           end-evaluate.
 
      ***---
       TGRUPGDO-ERR SECTION.
           use after error procedure on tgrupgdo.
           evaluate status-tgrupgdo
           when "35" continue
           when "39"
                display message "File [TGRUPGDO] Mismatch size!"
                           title titolo
                            icon 3
                
           when "98"
                display message "[TGRUPGDO] Indexed file corrupt!"
                           title titolo
                            icon 3
                
           end-evaluate.
 
      ***---
       TNAZIONI-ERR SECTION.
           use after error procedure on tnazioni.
           evaluate status-tnazioni
           when "35" continue
           when "39"
                display message "File [TNAZIONI] Mismatch size!"
                           title titolo
                            icon 3
                
           when "98"
                display message "[TNAZIONI] Indexed file corrupt!"
                           title titolo
                            icon 3
                
           end-evaluate.
 
      ***---
       TCLA1ART-ERR SECTION.
           use after error procedure on tcla1art.
           evaluate status-tcla1art
           when "35" continue
           when "39"
                display message "File [TCLA1ART] Mismatch size!"
                           title titolo
                            icon 3
                
           when "98"
                display message "[TCLA1ART] Indexed file corrupt!"
                           title titolo
                            icon 3
                
           end-evaluate.
 
      ***---
       TUDM-ERR SECTION.
           use after error procedure on tudm.
           evaluate status-tudm
           when "35" continue
           when "39"
                display message "File [TUDM] Mismatch size!"
                           title titolo
                            icon 3
                
           when "98"
                display message "[TUDM] Indexed file corrupt!"
                           title titolo
                            icon 3
                
           end-evaluate.
 
      ***---
       TMAGAZ-ERR SECTION.
           use after error procedure on tmagaz.
           evaluate status-tmagaz
           when "35" continue
           when "39"
                display message "File [TMAGAZ] Mismatch size!"
                           title titolo
                            icon 3
                
           when "98"
                display message "[TMAGAZ] Indexed file corrupt!"
                           title titolo
                            icon 3
                
           end-evaluate.
 
      ***---
       TNOMEN-ERR SECTION.
           use after error procedure on tnomen.
           evaluate status-tnomen
           when "35" continue
           when "39"
                display message "File [TNOMEN] Mismatch size!"
                           title titolo
                            icon 3
                
           when "98"
                display message "[TNOMEN] Indexed file corrupt!"
                           title titolo
                            icon 3
                
           end-evaluate.
 
      ***---
       TIMBALLI-ERR SECTION.
           use after error procedure on timballi.
           evaluate status-timballi
           when "35" continue
           when "39"
                display message "File [TIMBALLI] Mismatch size!"
                           title titolo
                            icon 3
                
           when "98"
                display message "[TIMBALLI] Indexed file corrupt!"
                           title titolo
                            icon 3
                
           end-evaluate.
 
      ***---
       TVETTORI-ERR SECTION.
           use after error procedure on tvettori.
           evaluate status-tvettori
           when "35" continue
           when "39"
                display message "File [TVETTORI] Mismatch size!"
                           title titolo
                            icon 3
                
           when "98"
                display message "[TVETTORI] Indexed file corrupt!"
                           title titolo
                            icon 3
                
           end-evaluate.
 
      ***---
       TPROV-ERR SECTION.
           use after error procedure on tprov.
           evaluate status-tprov
           when "35" continue
           when "39"
                display message "File [TPROV] Mismatch size!"
                           title titolo
                            icon 3
                
           when "98"
                display message "[TPROV] Indexed file corrupt!"
                           title titolo
                            icon 3
                
           end-evaluate.
 
      ***---
       TTIPOCLI-ERR SECTION.
           use after error procedure on ttipocli.
           evaluate status-ttipocli
           when "35" continue
           when "39"
                display message "File [TTIPOCLI] Mismatch size!"
                           title titolo
                            icon 3
                
           when "98"
                display message "[TTIPOCLI] Indexed file corrupt!"
                           title titolo
                            icon 3
                
           end-evaluate.
 
      ***---
       AGENTI-ERR SECTION.
           use after error procedure on agenti.
           evaluate status-agenti
           when "35" continue
           when "39"
                display message "File [AGENTI] Mismatch size!"
                           title titolo
                            icon 3
                
           when "98"
                display message "[AGENTI] Indexed file corrupt!"
                           title titolo
                            icon 3
                
           end-evaluate.
 
      ***---
       CLIENTI-ERR SECTION.
           use after error procedure on clienti.
           evaluate status-clienti
           when "35" continue
           when "39"
                display message "File [CLIENTI] Mismatch size!"
                           title titolo
                            icon 3
                
           when "98"
                display message "[CLIENTI] Indexed file corrupt!"
                           title titolo
                            icon 3
                
           end-evaluate.
 
      ***---
       DESTINI-ERR SECTION.
           use after error procedure on destini.
           evaluate status-destini
           when "35" continue
           when "39"
                display message "File [DESTINI] Mismatch size!"
                           title titolo
                            icon 3
                
           when "98"
                display message "[DESTINI] Indexed file corrupt!"
                           title titolo
                            icon 3
                
           end-evaluate.
 
      ***---
       DESTINIF-ERR SECTION.
           use after error procedure on destinif.
           evaluate status-destinif
           when "35" continue
           when "39"
                display message "File [DESTINIF] Mismatch size!"
                           title titolo
                            icon 3
                
           when "98"
                display message "[DESTINIF] Indexed file corrupt!"
                           title titolo
                            icon 3
                
           end-evaluate.
 
      ***---
       RECAPITI-ERR SECTION.
           use after error procedure on recapiti.
           evaluate status-recapiti
           when "35" continue
           when "39"
                display message "File [RECAPITI] Mismatch size!"
                           title titolo
                            icon 3
                
           when "98"
                display message "[RECAPITI] Indexed file corrupt!"
                           title titolo
                            icon 3
                
           end-evaluate.
 
      ***---
       TSETMERC-ERR SECTION.
           use after error procedure on tsetmerc.
           evaluate status-tsetmerc
           when "35" continue
           when "39"
                display message "File [TSETMERC] Mismatch size!"
                           title titolo
                            icon 3
                
           when "98"
                display message "[TSETMERC] Indexed file corrupt!"
                           title titolo
                            icon 3
                
           end-evaluate.
 
      ***---
       NOTE-ERR SECTION.
           use after error procedure on note.
           evaluate status-note
           when "35" continue
           when "39"
                display message "File [NOTE] Mismatch size!"
                           title titolo
                            icon 3
                
           when "98"
                display message "[NOTE] Indexed file corrupt!"
                           title titolo
                            icon 3
                
           end-evaluate.
 
      ***---
       TMARCHE-ERR SECTION.
           use after error procedure on tmarche.
           evaluate status-tmarche
           when "35" continue
           when "39"
                display message "File [TMARCHE] Mismatch size!"
                           title titolo
                            icon 3
                
           when "98"
                display message "[TMARCHE] Indexed file corrupt!"
                           title titolo
                            icon 3
                
           end-evaluate.
 
      ***---
       TCODPAG-ERR SECTION.
           use after error procedure on tcodpag.
           evaluate status-tcodpag
           when "35" continue
           when "39"
                display message "File [TCODPAG] Mismatch size!"
                           title titolo
                            icon 3
                
           when "98"
                display message "[TCODPAG] Indexed file corrupt!"
                           title titolo
                            icon 3


                
           end-evaluate.
 
      ***---
       TIVAESE-ERR SECTION.
           use after error procedure on tivaese.
           evaluate status-tivaese
           when "35" continue
           when "39"
                display message "File [TIVAESE] Mismatch size!"
                           title titolo
                            icon 3
                
           when "98"
                display message "[TIVAESE] Indexed file corrupt!"
                           title titolo
                            icon 3
                
           end-evaluate.
 
      ***---
       TIMPOSTE-ERR SECTION.
           use after error procedure on timposte.
           evaluate status-timposte
           when "35" continue
           when "39"
                display message "File [TIMPOSTE] Mismatch size!"
                           title titolo
                            icon 3
                
           when "98"
                display message "[TIMPOSTE] Indexed file corrupt!"
                           title titolo
                            icon 3
                
           end-evaluate.
 
      ***---
       ARTICOLI-ERR SECTION.
           use after error procedure on articoli.
           evaluate status-articoli
           when "35" continue
           when "39"
                display message "File [ARTICOLI] Mismatch size!"
                           title titolo
                            icon 3
                
           when "98"
                display message "[ARTICOLI] Indexed file corrupt!"
                           title titolo
                            icon 3
                
           end-evaluate.
 
      ***---
       ASSORCLI-ERR SECTION.
           use after error procedure on assorcli.
           evaluate status-assorcli
           when "35" continue
           when "39"
                display message "File [ASSORCLI] Mismatch size!"
                           title titolo
                            icon 3
                
           when "98"
                display message "[ASSORCLI] Indexed file corrupt!"
                           title titolo
                            icon 3
                
           end-evaluate.
 
      ***---
       PROGMAG-ERR SECTION.
           use after error procedure on progmag.
           evaluate status-progmag
           when "35" continue
           when "39"
                display message "File [PROGMAG] Mismatch size!"
                           title titolo
                            icon 3
                
           when "98"
                display message "[PROGMAG] Indexed file corrupt!"
                           title titolo
                            icon 3
                
           end-evaluate.
 
      ***---
       PROGMAGRIC-ERR SECTION.
           use after error procedure on progmagric.
           evaluate status-progmagric
           when "35" continue
           when "39"
                display message "File [PROGMAGRIC] Mismatch size!"
                           title titolo
                            icon 3
                
           when "98"
                display message "[PROGMAGRIC] Indexed file corrupt!"
                           title titolo
                            icon 3
                
           end-evaluate.
 
      ***---
       TORDINI-ERR SECTION.
           use after error procedure on tordini.
           evaluate status-tordini
           when "35" continue
           when "39"
                display message "File [TORDINI] Mismatch size!"
                           title titolo
                            icon 3
                
           when "98"
                display message "[TORDINI] Indexed file corrupt!"
                           title titolo
                            icon 3
                
           end-evaluate.
 
      ***---
       RORDINI-ERR SECTION.
           use after error procedure on rordini.
           evaluate status-rordini
           when "35" continue
           when "39"
                display message "File [RORDINI] Mismatch size!"
                           title titolo
                            icon 3
                

           when "98"
                display message "[RORDINI] Indexed file corrupt!"
                           title titolo
                            icon 3
                
           end-evaluate.
 
      ***---
       EORDINI-ERR SECTION.
           use after error procedure on eordini.
           evaluate status-eordini
           when "35" continue
           when "39"
                display message "File [EORDINI] Mismatch size!"
                           title titolo
                            icon 3
                
           when "98"
                display message "[EORDINI] Indexed file corrupt!"


                           title titolo
                            icon 3
                
           end-evaluate.
 
      ***---
      ***** INT-TORDINI-ERR SECTION.
      *****     use after error procedure on int-tordini.
      *****     evaluate status-int-tordini
      *****     when "35" continue
      *****     when "39"
      *****          display message "File [INT-TORDINI] Mismatch size!"
      *****                     title titolo
      *****                      icon 3
      *****          
      *****     when "98"
      *****          display message "[INT-TORDINI] Indexed file corrupt!"
      *****                     title titolo
      *****                      icon 3
      *****          
      *****     end-evaluate.
      *****
      ********---
      ***** INT-RORDINI-ERR SECTION.
      *****     use after error procedure on int-rordini.
      *****     evaluate status-int-rordini
      *****     when "35" continue
      *****     when "39"
      *****          display message "File [INT-RORDINI] Mismatch size!"
      *****                     title titolo
      *****                      icon 3
      *****          
      *****     when "98"
      *****          display message "[INT-RORDINI] Indexed file corrupt!"
      *****                     title titolo
      *****                      icon 3
      *****          
      *****     end-evaluate.
 
      ********---
      ***** BTORDINI-ERR SECTION.
      *****     use after error procedure on btordini.
      *****     evaluate status-btordini
      *****     when "35" continue
      *****     when "39"
      *****          display message "File [BTORDINI] Mismatch size!"
      *****                     title titolo
      *****                      icon 3
      *****          
      *****     when "98"
      *****          display message "[BTORDINI] Indexed file corrupt!"
      *****                     title titolo
      *****                      icon 3
      *****          
      *****     end-evaluate.
 
      ********---
      ***** BRORDINI-ERR SECTION.
      *****     use after error procedure on brordini.
      *****     evaluate status-brordini
      *****     when "35" continue
      *****     when "39"
      *****          display message "File [BRORDINI] Mismatch size!"
      *****                     title titolo
      *****                      icon 3
      *****          
      *****     when "98"
      *****          display message "[BRORDINI] Indexed file corrupt!"
      *****                     title titolo
      *****                      icon 3
      *****          
      *****     end-evaluate.
 
      ***---
       TNOTACR-ERR SECTION.
           use after error procedure on tnotacr.
           evaluate status-tnotacr
           when "35" continue
           when "39"
                display message "File [TNOTACR] Mismatch size!"
                           title titolo
                            icon 3
                
           when "98"
                display message "[TNOTACR] Indexed file corrupt!"
                           title titolo
                            icon 3
                
           end-evaluate.
 
      ***---
       RNOTACR-ERR SECTION.
           use after error procedure on rnotacr.
           evaluate status-rnotacr
           when "35" continue
           when "39"
                display message "File [RNOTACR] Mismatch size!"
                           title titolo
                            icon 3
                
           when "98"
                display message "[RNOTACR] Indexed file corrupt!"
                           title titolo
                            icon 3
                
           end-evaluate.
 
      ***---
       BTNOTACR-ERR SECTION.
           use after error procedure on btnotacr.
           evaluate status-btnotacr
           when "35" continue
           when "39"
                display message "File [BTNOTACR] Mismatch size!"
                           title titolo
                            icon 3
                
           when "98"
                display message "[BTNOTACR] Indexed file corrupt!"
                           title titolo
                            icon 3
                
           end-evaluate.
 
      ***---
       BRNOTACR-ERR SECTION.
           use after error procedure on brnotacr.
           evaluate status-brnotacr
           when "35" continue
           when "39"
                display message "File [BRNOTACR] Mismatch size!"
                           title titolo
                            icon 3
                
           when "98"
                display message "[BRNOTACR] Indexed file corrupt!"
                           title titolo
                            icon 3
                
           end-evaluate.
 
      ***---
       TCONTAT-ERR SECTION.
           use after error procedure on tcontat.
           evaluate status-tcontat
           when "35" continue
           when "39"
                display message "File [TCONTAT] Mismatch size!"
                           title titolo
                            icon 3
                
           when "98"
                display message "[TCONTAT] Indexed file corrupt!"
                           title titolo
                            icon 3
                
           end-evaluate.
 
      ***---
       TCAUMAG-ERR SECTION.
           use after error procedure on tcaumag.
           evaluate status-tcaumag
           when "35" continue
           when "39"
                display message "File [TCAUMAG] Mismatch size!"
                           title titolo
                            icon 3
                
           when "98"
                display message "[TCAUMAG] Indexed file corrupt!"
                           title titolo
                            icon 3
                
           end-evaluate.
 
      ***---
       TMOVMAG-ERR SECTION.
           use after error procedure on tmovmag.
           evaluate status-tmovmag
           when "35" continue
           when "39"
                display message "File [TMOVMAG] Mismatch size!"
                           title titolo
                            icon 3
                
           when "98"
                display message "[TMOVMAG] Indexed file corrupt!"
                           title titolo
                            icon 3
                
           end-evaluate.
 
      ***---
       RMOVMAG-ERR SECTION.
           use after error procedure on rmovmag.
           evaluate status-rmovmag
           when "35" continue
           when "39"
                display message "File [RMOVMAG] Mismatch size!"
                           title titolo
                            icon 3
                
           when "98"
                display message "[RMOVMAG] Indexed file corrupt!"
                           title titolo
                            icon 3
                
           end-evaluate.


 
      ***---
       TIMBALQTA-ERR SECTION.
           use after error procedure on timbalqta.
           evaluate status-timbalqta
           when "35" continue
           when "39"
                display message "File [TIMBALQTA] Mismatch size!"
                           title titolo
                            icon 3
                
           when "98"
                display message "[TIMBALQTA] Indexed file corrupt!"
                           title titolo
                            icon 3
                
           end-evaluate.
 
      ***---
       TPARAMGE-ERR SECTION.
           use after error procedure on tparamge.
           evaluate status-tparamge
           when "35" continue
           when "39"
                display message "File [TPARAMGE] Mismatch size!"
                           title titolo
                            icon 3
                
           when "98"
                display message "[TPARAMGE] Indexed file corrupt!"
                           title titolo
                            icon 3
                
           end-evaluate.
 
      ***---
       TPARAMGE2-ERR SECTION.
           use after error procedure on tparamge2.
           evaluate status-tparamge2
           when "35" continue
           when "39"
                display message "File [TPARAMGE2] Mismatch size!"
                           title titolo
                            icon 3
                
           when "98"
                display message "[TPARAMGE2] Indexed file corrupt!"
                           title titolo
                            icon 3
                
           end-evaluate.
 
      ***---
       DISTINTEB-ERR SECTION.
           use after error procedure on distinteb.
           evaluate status-distinteb
           when "35" continue
           when "39"
                display message "File [DISTINTEB] Mismatch size!"
                           title titolo
                            icon 3
                
           when "98"
                display message "[DISTINTEB] Indexed file corrupt!"
                           title titolo
                            icon 3
                
           end-evaluate.
 
      ***---
       TCAUDISB-ERR SECTION.

           use after error procedure on tcaudisb.
           evaluate status-tcaudisb
           when "35" continue
           when "39"
                display message "File [TCAUDISB] Mismatch size!"
                           title titolo
                            icon 3
                
           when "98"
                display message "[TCAUDISB] Indexed file corrupt!"
                           title titolo
                            icon 3
                
           end-evaluate.
 
      ***---
       STATRAFF-ERR SECTION.
           use after error procedure on statraff.
           evaluate status-statraff
           when "35" continue
           when "39"
                display message "File [STATRAFF] Mismatch size!"
                           title titolo
                            icon 3
                
           when "98"
                display message "[STATRAFF] Indexed file corrupt!"
                           title titolo
                            icon 3
                
           end-evaluate.
 
      ***---
       STATSETT-ERR SECTION.
           use after error procedure on statsett.
           evaluate status-statsett
           when "35" continue
           when "39"
                display message "File [STATSETT] Mismatch size!"
                           title titolo
                            icon 3
                
           when "98"
                display message "[STATSETT] Indexed file corrupt!"
                           title titolo
                            icon 3
                
           end-evaluate.
 
      ***---
       GIORMAG-ERR SECTION.
           use after error procedure on giormag.
           evaluate status-giormag
           when "35" continue
           when "39"
                display message "File [GIORMAG] Mismatch size!"
                           title titolo
                            icon 3
                
           when "98"
                display message "[GIORMAG] Indexed file corrupt!"
                           title titolo
                            icon 3
                
           end-evaluate.
 
      ***---
       TGIORMAG-ERR SECTION.
           use after error procedure on tgiormag.
           evaluate status-tgiormag
           when "35" continue
           when "39"
                display message "File [TGIORMAG] Mismatch size!"
                           title titolo
                            icon 3
                
           when "98"
                display message "[TGIORMAG] Indexed file corrupt!"
                           title titolo
                            icon 3
                
           end-evaluate.
 
      ***---
       ORDFOR-ERR SECTION.
           use after error procedure on ordfor.
           evaluate status-ordfor
           when "35" continue
           when "39"
                display message "File [ORDFOR] Mismatch size!"
                           title titolo
                            icon 3
                
           when "98"
                display message "[ORDFOR] Indexed file corrupt!"
                           title titolo
                            icon 3
                
           end-evaluate.
 
      ***---
       ORDFOR2-ERR SECTION.
           use after error procedure on ordfor2.
           evaluate status-ordfor2
           when "35" continue
           when "39"
                display message "File [ORDFOR2] Mismatch size!"
                           title titolo
                            icon 3
                
           when "98"
                display message "[ORDFOR2] Indexed file corrupt!"
                           title titolo
                            icon 3
                
           end-evaluate.
 
      ***---
       ORDFOR-OLD-ERR SECTION.
           use after error procedure on ordfor-old.
           evaluate status-ordfor-old
           when "35" continue
           when "39"
                display message "File [ORDFOR-OLD] Mismatch size!"
                           title titolo
                            icon 3
                
           when "98"
                display message "[ORDFOR-OLD] Indexed file corrupt!"
                           title titolo
                            icon 3
                
           end-evaluate.
 
      ***---
       MOVUTF-ERR SECTION.
           use after error procedure on movutf.
           evaluate status-movutf
           when "35" continue
           when "39"
                display message "File [MOVUTF] Mismatch size!"
                           title titolo
                            icon 3
                
           when "98"
                display message "[MOVUTF] Indexed file corrupt!"
                           title titolo
                            icon 3
                
           end-evaluate.
 
      ***---
       ANAUTF-ERR SECTION.
           use after error procedure on anautf.
           evaluate status-anautf
           when "35" continue
           when "39"
                display message "File [ANAUTF] Mismatch size!"
                           title titolo
                            icon 3
                
           when "98"
                display message "[ANAUTF] Indexed file corrupt!"
                           title titolo
                            icon 3
                
           end-evaluate.
 
      ***---
       TMOVTRAT-ERR SECTION.
           use after error procedure on tmovtrat.
           evaluate status-tmovtrat
           when "35" continue
           when "39"
                display message "File [TMOVTRAT] Mismatch size!"
                           title titolo
                            icon 3
                
           when "98"
                display message "[TMOVTRAT] Indexed file corrupt!"
                           title titolo
                            icon 3
                
           end-evaluate.
 
      ***---
       LISAGENTE-ERR SECTION.
           use after error procedure on lisagente.
           evaluate status-lisagente
           when "35" continue
           when "39"
                display message "File [LISAGENTE] Mismatch size!"
                           title titolo
                            icon 3
                
           when "98"
                display message "[LISAGENTE] Indexed file corrupt!"
                           title titolo
                            icon 3
                
           end-evaluate.
 
      ***---
       PROVVIG-ERR SECTION.
           use after error procedure on provvig.
           evaluate status-provvig
           when "35" continue
           when "39"
                display message "File [PROVVIG] Mismatch size!"
                           title titolo
                            icon 3
                
           when "98"
                display message "[PROVVIG] Indexed file corrupt!"
                           title titolo
                            icon 3
                
           end-evaluate.
 
      ***---
       TARIFVET-ERR SECTION.
           use after error procedure on tarifvet.
           evaluate status-tarifvet
           when "35" continue
           when "39"
                display message "File [TARIFVET] Mismatch size!"
                           title titolo
                            icon 3
                
           when "98"
                display message "[TARIFVET] Indexed file corrupt!"
                           title titolo
                            icon 3
                
           end-evaluate.
 
      ***---
       TRASPORTI-ERR SECTION.
           use after error procedure on trasporti.
           evaluate status-trasporti
           when "35" continue
           when "39"
                display message "File [TRASPORTI] Mismatch size!"
                           title titolo
                            icon 3
                
           when "98"
                display message "[TRASPORTI] Indexed file corrupt!"
                           title titolo
                            icon 3
                
           end-evaluate.
 
      ***---
       STATMESE-ERR SECTION.
           use after error procedure on statmese.
           evaluate status-statmese
           when "35" continue
           when "39"
                display message "File [STATMESE] Mismatch size!"
                           title titolo
                            icon 3
                
           when "98"
                display message "[STATMESE] Indexed file corrupt!"
                           title titolo
                            icon 3
                
           end-evaluate.
 
      ***---
       G2-ERR SECTION.
           use after error procedure on G2.
           evaluate status-G2
           when "35" continue
           when "39"
                display message "File [G2] Mismatch size!"
                           title titolo
                            icon 3
                
           when "98"
                display message "[G2] Indexed file corrupt!"
                           title titolo
                            icon 3
                
           end-evaluate.
 
      ***---
       TCONVANNO-ERR SECTION.
           use after error procedure on tconvanno.
           evaluate status-tconvanno
           when "35" continue
           when "39"
                display message "File [TCONVANNO] Mismatch size!"
                           title titolo
                            icon 3
                
           when "98"
                display message "[TCONVANNO] Indexed file corrupt!"
                           title titolo
                            icon 3
                
           end-evaluate.
            
      ***---
       LOCKFILE-ERR SECTION.
           use after error procedure on lockfile.
           evaluate status-lockfile
           when "35" continue
           when "39"
                display message "File [LOCKFILE] Mismatch size!"
                           title titolo
                            icon 3
                
           when "98"
                display message "[LOCKFILE] Indexed file corrupt!"
                           title titolo
                            icon 3
                
           end-evaluate.

      ***---
       LOCKNAME-ERR SECTION.
           use after error procedure on lockname.
           evaluate status-lockname
           when "35" continue
           when "39"
                display message "File [LOCKNAME] Mismatch size!"
                           title titolo
                            icon 3
                
           when "98"
                display message "[LOCKNAME] Indexed file corrupt!"
                           title titolo
                            icon 3
                
           end-evaluate.
 
      ***---
       LOGFILE-ERR SECTION.
           use after error procedure on logfile.
           evaluate status-logfile
           when "35" continue
           when "39"
                display message "File [LOGFILE] Mismatch size!"
                           title titolo
                            icon 3
                
           when "98"
                display message "[LOGFILE] Indexed file corrupt!"
                           title titolo
                            icon 3
                
           end-evaluate.
 
      ***---
       TPIOMBO-ERR SECTION.
           use after error procedure on tpiombo.
           evaluate status-tpiombo
           when "35" continue
           when "39"
                display message "File [TPIOMBO] Mismatch size!"
                           title titolo
                            icon 3
                
           when "98"
                display message "[TPIOMBO] Indexed file corrupt!"
                           title titolo
                            icon 3
                
           end-evaluate.
 
      ***---
       VETTEL-ERR SECTION.
           use after error procedure on vettel.
           evaluate status-vettel
           when "35" continue
           when "39"
                display message "File [VETTEL] Mismatch size!"
                           title titolo
                            icon 3
                
           when "98"
                display message "[VETTEL] Indexed file corrupt!"
                           title titolo
                            icon 3
                
           end-evaluate.
 
      ***---
       TESCONS-ERR SECTION.
           use after error procedure on tescons.
           evaluate status-tescons
           when "35" continue
           when "39"
                display message "File [TESCONS] Mismatch size!"
                           title titolo
                            icon 3
                
           when "98"
                display message "[TESCONS] Indexed file corrupt!"
                           title titolo
                            icon 3
                
           end-evaluate.
 
      ***---
       TESCONSVET-ERR SECTION.
           use after error procedure on tesconsvet.
           evaluate status-tesconsvet
           when "35" continue
           when "39"
                display message "File [TESCONSVET] Mismatch size!"
                           title titolo
                            icon 3
                
           when "98"
                display message "[TESCONSVET] Indexed file corrupt!"
                           title titolo
                            icon 3
                
           end-evaluate.
 
      ***---
       TAGLI-ERR SECTION.
           use after error procedure on tagli.
           evaluate status-tagli
           when "35" continue
           when "39"
                display message "File [TAGLI] Mismatch size!"
                           title titolo
                            icon 3
                
           when "98"
                display message "[TAGLI] Indexed file corrupt!"
                           title titolo
                            icon 3
                
           end-evaluate.
 
      ***---
       LISTINI-ERR SECTION.
           use after error procedure on listini.
           evaluate status-listini
           when "35" continue
           when "39"
                display message "File [LISTINI] Mismatch size!"
                           title titolo
                            icon 3
                
           when "98"
                display message "[LISTINI] Indexed file corrupt!"
                           title titolo
                            icon 3
                
           end-evaluate.
 
      ***---
       TPROMO-ERR SECTION.
           use after error procedure on tpromo.
           evaluate status-tpromo
           when "35" continue
           when "39"
                display message "File [TPROMO] Mismatch size!"
                           title titolo
                            icon 3
                
           when "98"
                display message "[TPROMO] Indexed file corrupt!"
                           title titolo
                            icon 3
                
           end-evaluate.
 
      ***---
       RPROMO-ERR SECTION.
           use after error procedure on rpromo.
           evaluate status-rpromo
           when "35" continue
           when "39"
                display message "File [RPROMO] Mismatch size!"
                           title titolo
                            icon 3

           when "98"
                display message "[RPROMO] Indexed file corrupt!"
                           title titolo
                            icon 3

           end-evaluate.
 
      ***---
       USR-TEL-ERR SECTION.
           use after error procedure on usr-tel.
           evaluate status-usr-tel
           when "35" continue
           when "39"
                display message "File [USR-TEL] Mismatch size!"
                           title titolo
                            icon 3

           when "98"
                display message "[USR-TEL] Indexed file corrupt!"
                           title titolo
                            icon 3

           end-evaluate.

      ***---
       LOCALI-ERR SECTION.
           use after error procedure on locali.
           evaluate status-locali
           when "35" continue
           when "39"
                display message "File [LOCALI] Mismatch size!"
                           title titolo
                            icon 3

           when "98"
                display message "[LOCALI] Indexed file corrupt!"
                           title titolo
                            icon 3

           end-evaluate.
 
      ***---
       BLISTER-ERR SECTION.
           use after error procedure on blister.
           evaluate status-blister
           when "35" continue
           when "39"
                display message "File [BLISTER] Mismatch size!"
                           title titolo
                            icon 3
                
           when "98"
                display message "[BLISTER] Indexed file corrupt!"
                           title titolo
                            icon 3
                
           end-evaluate.
 
      ***---
       NOTE-CONT-ERR SECTION.
           use after error procedure on note-cont.
           evaluate status-note-cont
           when "35" continue
           when "39"
                display message "File [NOTE-CONT] Mismatch size!"
                           title titolo
                            icon 3
                

           when "98"
                display message "[NOTE-CONT] Indexed file corrupt!"
                           title titolo
                            icon 3
                
           end-evaluate.
 
      ***---
       CONTESTAZIONI-ERR SECTION.
           use after error procedure on contestazioni.
           evaluate status-contestazioni
           when "35" continue
           when "39"
                display message "File [CONTESTAZIONI] Mismatch size!"
                           title titolo
                            icon 3
                
           when "98"
                display message "[CONTESTAZIONI] Indexed file corrupt!"
                           title titolo
                            icon 3
                
           end-evaluate.

      ***---
       TSCORTE-ERR SECTION.
           use after error procedure on tscorte.
           evaluate status-tscorte
           when "35" continue
           when "39"
                display message "File [TSCORTE] Mismatch size!"
                           title titolo
                            icon 3
                
           when "98"
                display message "[TSCORTE] Indexed file corrupt!"
                           title titolo
                            icon 3
                
           end-evaluate.

      ***---
       TLISTINI-ERR SECTION.
           use after error procedure on tlistini.
           evaluate status-tlistini
           when "35" continue
           when "39"
                display message "File [TLISTINI] Mismatch size!"
                           title titolo
                            icon 3
                
           when "98"
                display message "[TLISTINI] Indexed file corrupt!"
                           title titolo
                            icon 3
                
           end-evaluate.

      ***---
       RLISTINI-ERR SECTION.
           use after error procedure on rlistini.
           evaluate status-rlistini
           when "35" continue
           when "39"
                display message "File [RLISTINI] Mismatch size!"
                           title titolo
                            icon 3
                
           when "98"
                display message "[RLISTINI] Indexed file corrupt!"
                           title titolo
                            icon 3
                
           end-evaluate.

      ***---
       NFORN-ERR SECTION.
           use after error procedure on nforn.
           evaluate status-nforn
           when "35" continue
           when "39"
                display message "File [NFORN] Mismatch size!"
                           title titolo
                            icon 3
                
           when "98"
                display message "[NFORN] Indexed file corrupt!"
                           title titolo
                            icon 3
                
           end-evaluate.

      ***---
       NFORN-DEST-ERR SECTION.
           use after error procedure on nforn-dest.
           evaluate status-nforn-dest
           when "35" 
                continue
           when "39"
                display message "File [NFORN-DEST] Mismatch size!"
                           title titolo
                            icon 3
                
           when "98"
                display message "[NFORN-DEST] Indexed file corrupt!"
                           title titolo
                            icon 3
                
           end-evaluate.

      ***---
       TTIPOAVV-ERR SECTION.
           use after error procedure on ttipoavv.
           evaluate status-ttipoavv
           when "35" continue
           when "39"
                display message "File [TTIPOAVV] Mismatch size!"
                           title titolo
                            icon 3
                
           when "98"
                display message "[TTIPOAVV] Indexed file corrupt!"
                           title titolo
                            icon 3
                
           end-evaluate.

      ***---
       USERAVV-ERR SECTION.
           use after error procedure on useravv.
           evaluate status-useravv
           when "35" continue
           when "39"
                display message "File [USERAVV] Mismatch size!"
                           title titolo
                            icon 3
                
           when "98"
                display message "[USERAVV] Indexed file corrupt!"
                           title titolo
                            icon 3
                
           end-evaluate.
 
      ***---
       TORDFORN-ERR SECTION.
           use after error procedure on TORDFORN.
           evaluate status-TORDFORN
           when "35" continue
           when "39"
                display message "File [TORDFORN] Mismatch size!"
                           title titolo
                            icon 3
                
           when "98"
                display message "[TORDFORN] Indexed file corrupt!"
                           title titolo
                            icon 3
                
           end-evaluate. 
 
      ***---
       RORDFORN-ERR SECTION.
           use after error procedure on RORDFORN.
           evaluate status-RORDFORN
           when "35" continue
           when "39"
                display message "File [RORDFORN] Mismatch size!"
                           title titolo
                            icon 3
                
           when "98"
                display message "[RORDFORN] Indexed file corrupt!"
                           title titolo
                            icon 3
                
           end-evaluate.

      ***---
       SORDFORN-ERR SECTION.
           use after error procedure on SORDFORN.
           evaluate status-SORDFORN
           when "35" continue
           when "39"
                display message "File [SORDFORN] Mismatch size!"
                           title titolo
                            icon 3
                
           when "98"
                display message "[SORDFORN] Indexed file corrupt!"
                           title titolo
                            icon 3
                
           end-evaluate.

      ***---
       CATART-ERR SECTION.
           use after error procedure on CATART.
           evaluate status-catart
           when "35" continue
           when "39"
                display message "File [CATART] Mismatch size!"
                           title titolo
                            icon 3
                
           when "98"
                display message "[CATART] Indexed file corrupt!"
                           title titolo
                            icon 3
                
           end-evaluate.

      ***---
       PROMOEVA-ERR SECTION.
           use after error procedure on PROMOEVA.
           evaluate status-promoeva
           when "35" continue
           when "39"
                display message "File [PROMOEVA] Mismatch size!"
                           title titolo
                            icon 3
                
           when "98"
                display message "[PROMOEVA] Indexed file corrupt!"
                           title titolo
                            icon 3
                
           end-evaluate.

      ***---
       TPARAMEVA-ERR SECTION.
           use after error procedure on TPARAMEVA.
           evaluate status-tparameva
           when "35" continue
           when "39"
                display message "File [TPARAMEVA] Mismatch size!"
                           title titolo
                            icon 3
                
           when "98"
                display message "[TPARAMEVA] Indexed file corrupt!"
                           title titolo
                            icon 3
                
           end-evaluate.

      ***---
      ***** EVACLIDES-ERR SECTION.
      *****     use after error procedure on EVACLIDES.
      *****     evaluate status-evaclides
      *****     when "35" continue
      *****     when "39"
      *****          display message "File [EVACLIDES] Mismatch size!"
      *****                     title titolo
      *****                      icon 3
      *****          
      *****     when "98"
      *****          display message "[EVACLIDES] Indexed file corrupt!"
      *****                     title titolo
      *****                      icon 3
      *****          
      *****     end-evaluate.

      ***---
       PGMEXE-ERR SECTION.
           use after error procedure on PGMEXE.
           evaluate status-pgmexe
           when "35" continue
           when "39"
                display message "File [PGMEXE] Mismatch size!"
                           title titolo
                            icon 3
                
           when "98"
                display message "[PGMEXE] Indexed file corrupt!"
                           title titolo
                            icon 3
                
           end-evaluate.

      ***---
       RELTOR-ERR SECTION.
           use after error procedure on reltor.
           evaluate status-reltor
           when "35" continue
           when "39"
                display message "File [RELTOR] Mismatch size!"
                           title titolo
                            icon 3
                
           when "98"
                display message "[RELTOR] Indexed file corrupt!"
                           title titolo
                            icon 3
                
           end-evaluate.

      ***---
       COPERFAB-ERR SECTION.
           use after error procedure on coperfab.
           evaluate status-coperfab
           when "35" continue
           when "39"
                display message "File [COPERFAB] Mismatch size!"
                           title titolo
                            icon 3
                
           when "98"
                display message "[COPERFAB] Indexed file corrupt!"
                           title titolo
                            icon 3
                
           end-evaluate.

      ***---
       TEVA-ERR SECTION.
           use after error procedure on teva.
           evaluate status-teva
           when "35" continue
           when "39"
                display message "File [TEVA] Mismatch size!"
                           title titolo
                            icon 3
                
           when "98"
                display message "[TEVA] Indexed file corrupt!"
                           title titolo
                            icon 3
                
           end-evaluate.

      ***---
       REVA-ERR SECTION.
           use after error procedure on reva.
           evaluate status-reva
           when "35" continue
           when "39"
                display message "File [REVA] Mismatch size!"
                           title titolo
                            icon 3
                
           when "98"
                display message "[REVA] Indexed file corrupt!"
                           title titolo
                            icon 3
                
           end-evaluate.

      ***---
       ART-ORDFORN-ERR SECTION.
           use after error procedure on art-ordforn.
           evaluate status-art-ordforn
           when "35" continue
           when "39"
                display message "File [ART-ORDFORN] Mismatch size!"
                           title titolo
                            icon 3
                
           when "98"
                display message "[ART-ORDFORN] Indexed file corrupt!"
                           title titolo
                            icon 3
                
           end-evaluate.

      ***---
       QTA-VEND-ERR SECTION.
           use after error procedure on qta-vend.
           evaluate status-qta-vend
           when "35" continue
           when "39"
                display message "File [QTA-VEND] Mismatch size!"
                           title titolo
                            icon 3
                
           when "98"
                display message "[QTA-VEND] Indexed file corrupt!"
                           title titolo
                            icon 3
                
           end-evaluate.

      ***---
       MTORDINI-ERR SECTION.
           use after error procedure on mtordini.
           evaluate status-mtordini
           when "35" continue
           when "39"
                display message "File [MTORDINI] Mismatch size!"
                           title titolo
                            icon 3
                
           when "98"
                display message "[MTORDINI] Indexed file corrupt!"
                           title titolo
                            icon 3
                
           end-evaluate. 

      ***---
       MRORDINI-ERR SECTION.
           use after error procedure on mrordini.
           evaluate status-mrordini
           when "35" continue
           when "39"
                display message "File [MRORDINI] Mismatch size!"
                           title titolo
                            icon 3
                
           when "98"
                display message "[MRORDINI] Indexed file corrupt!"
                           title titolo
                            icon 3
                
           end-evaluate.

      ***---
       IMPFORN-ERR SECTION.
           use after error procedure on impforn.
           evaluate status-impforn
           when "35" continue
           when "39"
                display message "File [IMPFORN] Mismatch size!"
                           title titolo
                            icon 3
                
           when "98"
                display message "[IMPFORN] Indexed file corrupt!"
                           title titolo
                            icon 3
                
           end-evaluate.

      ***---
       SITFIN-ERR SECTION.
           use after error procedure on sitfin.
           evaluate status-sitfin
           when "35" continue
           when "39"
                display message "File [SITFIN] Mismatch size!"
                           title titolo
                            icon 3
                
           when "98"
                display message "[SITFIN] Indexed file corrupt!"
                           title titolo
                            icon 3
                
           end-evaluate.

      ***---
       PARAM-ERR SECTION.
           use after error procedure on param.
           evaluate status-param
           when "35" continue
           when "39"
                display message "File [PARAM] Mismatch size!"
                           title titolo
                            icon 3
                
           when "98"
                display message "[PARAM] Indexed file corrupt!"
                           title titolo
                            icon 3
                
           end-evaluate.

      ***---
       NLISTINI-ERR SECTION.
           use after error procedure on nlistini.
           evaluate status-nlistini
           when "35" continue
           when "39"
                display message "File [NLISTINI] Mismatch size!"
                           title titolo
                            icon 3
                
           when "98"
                display message "[NLISTINI] Indexed file corrupt!"
                           title titolo
                            icon 3
                
           end-evaluate.

      ***---
       ART-EXP-SHI-ERR SECTION.
           use after error procedure on art-exp-shi.
           evaluate status-art-exp-shi
           when "35" continue
           when "39"
                display message "File [ART-EXP-SHI] Mismatch size!"
                           title titolo
                            icon 3
                
           when "98"
                display message "[ART-EXP-SHI] Indexed file corrupt!"
                           title titolo
                            icon 3
                
           end-evaluate.
 
      ***---
       MULTIGEST-ERR SECTION.
           use after error procedure on multigest.
           evaluate status-multigest
           when "35" continue
           when "39"
                display message "File [MULTIGEST] Mismatch size!"
                           title titolo
                            icon 3
                
           when "98"
                display message "[MULTIGEST] Indexed file corrupt!"
                           title titolo
                            icon 3
                
           end-evaluate.
 
      ***---
       PARAMSHI-ERR SECTION.
           use after error procedure on paramshi.
           evaluate status-paramshi
           when "35" continue
           when "39"
                display message "File [PARAMSHI] Mismatch size!"
                           title titolo
                            icon 3
                
           when "98"
                display message "[PARAMSHI] Indexed file corrupt!"
                           title titolo
                            icon 3
                
           end-evaluate.
 
      ***---
       PARAMGET-ERR SECTION.
           use after error procedure on paramget.
           evaluate status-paramget
           when "35" continue
           when "39"
                display message "File [PARAMGET] Mismatch size!"
                           title titolo
                            icon 3
                
           when "98"
                display message "[PARAMGET] Indexed file corrupt!"
                           title titolo
                            icon 3
                
           end-evaluate.
 
      ***---
       CLI-PRG-ERR SECTION.
           use after error procedure on cli-prg.
           evaluate status-cli-prg
           when "35" continue
           when "39"
                display message "File [CLI-PRG] Mismatch size!"
                           title titolo
                            icon 3
                
           when "98"
                display message "[CLI-PRG] Indexed file corrupt!"
                           title titolo
                            icon 3
                
           end-evaluate.
 
      ***---
       CONTAB-ERR SECTION.
           use after error procedure on contab.
           evaluate status-contab
           when "35" continue
           when "39"
                display message "File [CONTAB] Mismatch size!"
                           title titolo
                            icon 3
                
           when "98"
                display message "[CONTAB] Indexed file corrupt!"
                           title titolo
                            icon 3
                
           end-evaluate.
              
      ***---
       PAGBLOC-ERR SECTION.
           use after error procedure on pagbloc.
           evaluate status-pagbloc
           when "35" continue
           when "39"
                display message "File [PAGBLOC] Mismatch size!"
                           title titolo
                            icon 3
                
           when "98"
                display message "[PAGBLOC] Indexed file corrupt!"
                           title titolo
                            icon 3
                
           end-evaluate.

      ***---
       TGRUPPI-ERR SECTION.
           use after error procedure on tgruppi.
           evaluate status-tgruppi
           when "35" continue
           when "39"
                display message "File [TGRUPPI] Mismatch size!"
                           title titolo
                            icon 3
                
           when "98"
                display message "[TGRUPPI] Indexed file corrupt!"
                           title titolo
                            icon 3
                
           end-evaluate.

      ***---
       TSETINVIO-ERR SECTION.
           use after error procedure on tsetinvio.
           evaluate status-tsetinvio
           when "35" continue                 
           when "39"
                display message "File [TSETINVIO] Mismatch size!"
                           title titolo
                            icon 3
                
           when "98"
                display message "[TSETINVIO] Indexed file corrupt!"
                           title titolo
                            icon 3
                
           end-evaluate.      

      ***---
       BATNOTT-ERR SECTION.
           use after error procedure on batnott.
           evaluate status-batnott
           when "35" continue
           when "39"
                display message "File [BATNOTT] Mismatch size!"
                           title titolo
                            icon 3
                
           when "98"
                display message "[BATNOTT] Indexed file corrupt!"
                           title titolo
                            icon 3
                
           end-evaluate.

      ***---
       stato-invio-ERR SECTION.
           use after error procedure on stato-invio.
           evaluate status-stato-invio
           when "35" continue
           when "39"
                display message "File [stato-invio] Mismatch size!"
                           title titolo
                            icon 3
                
           when "98"
                display message "[stato-invio] Indexed file corrupt!"
                           title titolo
                            icon 3
                
           end-evaluate.            

      ***---
       QTA-PORDINI-ERR SECTION.
           use after error procedure on qta-pordini.
           evaluate status-qta-pordini
           when "35" continue
           when "39"
                display message "File [QTA-PORDINI] Mismatch size!"
                           title titolo
                            icon 3
                
           when "98"
                display message "[QTA-PORDINI] Indexed file corrupt!"
                           title titolo
                            icon 3
                
           end-evaluate.

      ***---
       PARAMFEL-ERR SECTION.
           use after error procedure on paramfel.
           evaluate status-paramfel
           when "35" continue
           when "39"
                display message "File [PARAMFEL] Mismatch size!"
                           title titolo
                            icon 3
                
           when "98"
                display message "[PARAMFEL] Indexed file corrupt!"
                           title titolo
                            icon 3
                
           end-evaluate.        

      ***---
       TNUMORDF-ERR SECTION.
           use after error procedure on tnumordf.
           evaluate status-tnumordf
           when "35" continue
           when "39"
                display message "File [TNUMORDF] Mismatch size!"
                           title titolo
                            icon 3
                
           when "98"
                display message "[TNUMORDF] Indexed file corrupt!"
                           title titolo
                            icon 3
                
           end-evaluate.

      ***---
       GRADE-ERR SECTION.
           use after error procedure on grade.
           evaluate status-grade
           when "35" continue
           when "39"
                display message "File [GRADE] Mismatch size!"
                           title titolo
                            icon 3
                
           when "98"
                display message "[GRADE] Indexed file corrupt!"
                           title titolo
                            icon 3
                
           end-evaluate.

      ***---
       BATTSOST-ERR SECTION.
           use after error procedure on battsost.
           evaluate status-battsost
           when "35" continue
           when "39"
                display message "File [BATTSOST] Mismatch size!"
                           title titolo
                            icon 3
                
           when "98"
                display message "[BATTSOST] Indexed file corrupt!"
                           title titolo
                            icon 3
                
           end-evaluate.        

      ***---
       MACROBATCH-ERR SECTION.
           use after error procedure on macrobatch.
           evaluate status-macrobatch
           when "35" continue
           when "39"
                display message "File [MACROBATCH] Mismatch size!"
                           title titolo
                            icon 3
                
           when "98"
                display message "[MACROBATCH] Indexed file corrupt!"
                           title titolo
                            icon 3
                
           end-evaluate.

      ***---
       HLEB-ERR SECTION.
           use after error procedure on hleb.
           evaluate status-hleb
           when "35" continue
           end-evaluate.

      ***---
       ANACAP-ERR SECTION.
           use after error procedure on anacap.
           evaluate status-anacap
           when "35" continue
           when "39"
                display message "File [ANACAP] Mismatch size!"
                           title titolo
                            icon 3
                
           when "98"
                display message "[ANACAP] Indexed file corrupt!"
                           title titolo
                            icon 3
                
           end-evaluate.

      ***---
       LOCK-DIV-ERR SECTION.
           use after error procedure on lock-div.
           evaluate status-lock-div
           when "35" continue
           when "39"
                display message "File [LOCK-DIV] Mismatch size!"
                           title titolo
                            icon 3
                
           when "98"
                display message "[LOCK-DIV] Indexed file corrupt!"
                           title titolo
                            icon 3
                
           end-evaluate.

      ***---
       LOG4MAS-ERR SECTION.
           use after error procedure on log4mas.
           evaluate status-log4mas
           when "35" continue
           when "39"
                display message "File [LOG4MAS] Mismatch size!"
                           title titolo
                            icon 3
                
           when "98"
                display message "[LOG4MAS] Indexed file corrupt!"
                           title titolo
                            icon 3
                
           end-evaluate.

           copy "EDI-declaratives.cpy".

       END DECLARATIVES.

       MAIN-PRG.              
           accept SYSTEM-INFORMATION from system-info.
           move 0 to link-status.
      * Procedure on SSI files: se il file NON c'è NON devo crearlo, ma
      * produrre un messaggio d'errore. E' un modo per sapere con certezza 
      * che si sta puntando a quel file di SSI nella cartella apposita
           open input ABI.
           if status-abi not = "00"
              display message "ERROR ", status-abi," ON SSI FILE"
                       x"0d0a""[ABI] Not Found. "
                       x"0d0a""Contattare assistenza!"
                        title "ERROR!"
                         icon 3
              move -1 to link-status
              goback
           else
              close ABI
           end-if.

           open input ART.
           if status-art not = "00"
              display message "ERROR ", status-abi," ON SSI FILE"
                       x"0d0a""[ART] Not Found. "
                       x"0d0a""Contattare assistenza!"
                        title "ERROR!"
                         icon 3
              move -1 to link-status
              goback
           else
              close ART
           end-if.

      *****     open input CLI.
      *****     if status-cli not = "00"
      *****        display message "ERROR ", status-cli," ON SSI FILE"
      *****                 x"0d0a""[CLI] Not Found. "
      *****                 x"0d0a""Contattare assistenza!"
      *****                  title "ERROR!"
      *****                   icon 3
      *****        move -1 to link-status
      *****        goback
      *****     else
      *****        close CLI
      *****     end-if.

           open input DES.
           if status-des not = "00"
              display message "ERROR ", status-des," ON SSI FILE"
                       x"0d0a""[DES] Not Found. "
                       x"0d0a""Contattare assistenza!"
                        title "ERROR!"
                         icon 3
              move -1 to link-status
              goback
           else
              close DES
           end-if.

      *****     open input FRN.
      *****     if status-frn not = "00"
      *****        display message "ERROR ", status-frn," ON SSI FILE"
      *****                 x"0d0a""[FRN] Not Found. "
      *****                 x"0d0a""Contattare assistenza!"
      *****                  title "ERROR!"
      *****                   icon 3
      *****        move -1 to link-status
      *****        goback
      *****     else
      *****        close FRN
      *****     end-if.

           open input DOCDI.
           if status-docdi not = "00"
              display message "ERROR ", status-docdi," ON SSI FILE"
                       x"0d0a""[DOCDI] Not Found. "
                       x"0d0a""Contattare assistenza!"
                        title "ERROR!"
                         icon 3
              move -1 to link-status
              goback
           else
              close DOCDI
           end-if.

           open input DOCCN.
           if status-doccn not = "00"
              display message "ERROR ", status-doccn," ON SSI FILE"
                       x"0d0a""[DOCCN] Not Found. "
                       x"0d0a""Contattare assistenza!"
                        title "ERROR!"
                         icon 3
              move -1 to link-status
              goback
           else
              close DOCCN
           end-if.

           open input DOCES.
           if status-doces not = "00"
              display message "ERROR ", status-docdi," ON SSI FILE"
                       x"0d0a""[DOCES] Not Found. "
                       x"0d0a""Contattare assistenza!"
                        title "ERROR!"
                         icon 3
              move -1 to link-status
              goback
           else
              close DOCES
           end-if.

           open input TBLCO.
           if status-tblco not = "00"
              display message "ERROR ", status-tblco," ON SSI FILE"
                       x"0d0a""[TBLCO] Not Found. "
                       x"0d0a""Contattare assistenza!"
                        title "ERROR!"
                         icon 3
              move -1 to link-status
              goback
           else
              close TBLCO
           end-if.

           open input TBLDO.
           if status-tbldo not = "00"
              display message "ERROR ", status-tbldo," ON SSI FILE"
                       x"0d0a""[TBLDO] Not Found. "
                       x"0d0a""Contattare assistenza!"
                        title "ERROR!"
                         icon 3
              move -1 to link-status
              goback
           else
              close TBLDO
           end-if.

           open input TBLNA.
           if status-tblna not = "00"
              display message "ERROR ", status-tblna," ON SSI FILE"
                       x"0d0a""[TBLNA] Not Found. "
                       x"0d0a""Contattare assistenza!"
                        title "ERROR!"
                         icon 3
              move -1 to link-status
              goback
           else
              close TBLNA
           end-if.

           open input TBLCA.
           if status-tblca not = "00"
              display message "ERROR ", status-tblca," ON SSI FILE"
                       x"0d0a""[TBLCA] Not Found. "
                       x"0d0a""Contattare assistenza!"
                        title "ERROR!"
                         icon 3
              move -1 to link-status
              goback
           else
              close TBLCA
           end-if.

           open input TBLAG.
           if status-tblag not = "00"
              display message "ERROR ", status-tblag," ON SSI FILE"
                       x"0d0a""[TBLAG] Not Found. "
                       x"0d0a""Contattare assistenza!"
                        title "ERROR!"
                         icon 3
              move -1 to link-status
              goback
           else
              close TBLAG
           end-if.

           open input TBLTR.
           if status-tbltr not = "00"
              display message "ERROR ", status-tbltr," ON SSI FILE"
                       x"0d0a""[TBLTR] Not Found. "
                       x"0d0a""Contattare assistenza!"
                        title "ERROR!"
                         icon 3
              move -1 to link-status
              goback
           else
              close TBLTR
           end-if.

           open input TBLPC.
           if status-tblpc not = "00"
              display message "ERROR ", status-tblpc," ON SSI FILE"
                       x"0d0a""[TBLPC] Not Found. "
                       x"0d0a""Contattare assistenza!"
                        title "ERROR!"
                         icon 3
              move -1 to link-status
              goback
           else
              close TBLPC
           end-if.         

           open input TBLVA.
           if status-tblva not = "00"
              display message "ERROR ", status-tblva," ON SSI FILE"
                       x"0d0a""[TBLVA] Not Found. "
                       x"0d0a""Contattare assistenza!"
                        title "ERROR!"
                         icon 3
              move -1 to link-status
              goback
           else
              close TBLVA
           end-if.         

           open input TBLME.
           if status-tblme not = "00"
              display message "ERROR ", status-tblme," ON SSI FILE"
                       x"0d0a""[TBLME] Not Found. "
                       x"0d0a""Contattare assistenza!"
                        title "ERROR!"
                         icon 3
              move -1 to link-status
              goback
           else
              close TBLME
           end-if.       

           open input TBLCS.
           if status-tblcs not = "00"
              display message "ERROR ", status-tblcs," ON SSI FILE"
                       x"0d0a""[TBLCS] Not Found. "
                       x"0d0a""Contattare assistenza!"
                        title "ERROR!"
                         icon 3
              move -1 to link-status
              goback
           else
              close TBLCS
           end-if.

           open input FPGRUPPICS.
           if status-fpgruppics not = "00"
              display message "ERROR ", status-fpgruppics," ON SSI FILE"
                       x"0d0a""[FPGRUPPICS] Not Found. "
                       x"0d0a""Contattare assistenza!"
                        title "ERROR!"
                         icon 3
              move -1 to link-status
              goback
           else
              close FPGRUPPICS
           end-if.

           open input PAT.         
           if status-pat not = "00"
              display message "ERROR ", status-pat," ON SSI FILE"
                       x"0d0a""[PAT] Not Found. "
                       x"0d0a""Contattare assistenza!"
                        title "ERROR!"
                         icon 3
              move -1 to link-status
              goback
           else
              close PAT
           end-if.

           open input PAS.
           if status-pas not = "00"
              display message "ERROR ", status-pas," ON SSI FILE"
                       x"0d0a""[PAS] Not Found. "
                       x"0d0a""Contattare assistenza!"
                        title "ERROR!"
                         icon 3
              move -1 to link-status
              goback
           else
              close PAS
           end-if.

           open input PAR.
           if status-par not = "00"
              display message "ERROR ", status-par," ON SSI FILE"
                       x"0d0a""[PAR] Not Found. "
                       x"0d0a""Contattare assistenza!"
                        title "ERROR!"
                         icon 3
              move -1 to link-status
              goback
           else
              close PAR
           end-if.

           open input PNT.
           if status-pnt not = "00"
              display message "ERROR ", status-pnt," ON SSI FILE"
                       x"0d0a""[PNT] Not Found. "
                       x"0d0a""Contattare assistenza!"
                        title "ERROR!"
                         icon 3
              move -1 to link-status
              goback
           else
              close PNT
           end-if.
                       
           open input PNR.
           if status-pnr not = "00"
              display message "ERROR ", status-pnr," ON SSI FILE"
                       x"0d0a""[PNR] Not Found. "
                       x"0d0a""Contattare assistenza!"
                        title "ERROR!"
                         icon 3
              move -1 to link-status
              goback
           else
              close PNR
           end-if.

           open input PNI.
           if status-pni not = "00"
              display message "ERROR ", status-pni," ON SSI FILE"
                       x"0d0a""[PNI] Not Found. "
                       x"0d0a""Contattare assistenza!"
                        title "ERROR!"
                         icon 3
              move -1 to link-status
              goback
           else
              close PNI
           end-if.

      *****     open input MAZ.
      *****     if status-maz not = "00"
      *****        display message "ERROR ON SSI FILE"
      *****                 x"0d0a""[MAZ] Not Found. "
      *****                 x"0d0a""Contattare assistenza!"
      *****                  title "ERROR!"
      *****                   icon 3
      *****     else
      *****        close MAZ
      *****     end-if.
      *****
      *****     open input CLZ.
      *****     if status-clz not = "00"
      *****        display message "ERROR ON SSI FILE"
      *****                 x"0d0a""[CLZ] Not Found. "
      *****                 x"0d0a""Contattare assistenza!"
      *****                  title "ERROR!"
      *****                   icon 3
      *****     else
      *****        close CLZ
      *****     end-if.

      * Procedure on GESLUX Files
           open input GDVT.
           if status-gdvt = "35"
              open output GDVT
           end-if.
           close GDVT.

           open input GDVR.
           if status-gdvr = "35"
              open output GDVR
           end-if.
           close GDVR.

           open input Prog.
           if stat-Prog = "35"
              open output Prog
           end-if.
           close Prog.        
                                 
           open input USER.
           if status-USER = "35"
              open output user
              close       user
              call   "conv-user"
              cancel "conv-user"
              open input user
           end-if.
           close USER.       
           
           open input user-pgm.
           if status-user-pgm = "35"
              open output user-pgm
           end-if.
           close user-pgm.

           open input tregioni.
           if status-tregioni = "35"
              open output tregioni
           end-if.
           close tregioni.

           open input tgrupgdo.
           if status-tgrupgdo = "35"
              open output tgrupgdo
           end-if.
           close tgrupgdo.

           open input tnazioni.
           if status-tnazioni = "35"
              open output tnazioni
           end-if.
           close tnazioni.

           open input tcla1art.
           if status-tcla1art = "35"
              open output tcla1art
           end-if.
           close tcla1art.
                          
           open input tudm.
           if status-tudm = "35"
              open output tudm
           end-if.
           close tudm.

           open input tmagaz.
           if status-tmagaz = "35"
              open output tmagaz
           end-if.
           close tmagaz.

           open input tnomen.
           if status-tnomen = "35"
              open output tnomen
           end-if.
           close tnomen.

           open input timballi.
           if status-timballi = "35"
              open output timballi
           end-if.
           close timballi.

           open input tvettori.
           if status-tvettori = "35"
              open output tvettori
           end-if.
           close tvettori.

           open input tprov.
           if status-tprov = "35"
              open output tprov
           end-if.
           close tprov.

           open input ttipocli.
           if status-ttipocli = "35"
              open output ttipocli
           end-if.
           close ttipocli.

           open input agenti.
           if status-agenti = "35"
              open output agenti
           end-if.
           close agenti.

           open input clienti.
           if status-clienti = "35"
              open output clienti
           end-if.
           close clienti.

           open input recapiti.
           if status-recapiti = "35"
              open output recapiti
           end-if.
           close recapiti.

           open input destini.
           if status-destini = "35"
              open output destini
           end-if.
           close destini.    

           open input destinif.
           if status-destinif = "35"
              open output destinif
           end-if.
           close destinif.    

           open input tsetmerc.
           if status-tsetmerc = "35"
              open output tsetmerc                   
           end-if.
           close tsetmerc.    

           open input note.
           if status-note = "35"
              open output note
           end-if.
           close note.

           open input tmarche.
           if status-tmarche = "35"
              open output tmarche
           end-if.
           close tmarche.

           open input tcodpag.
           if status-tcodpag = "35"
              open output tcodpag
           end-if.
           close tcodpag.

           open input tivaese.

           if status-tivaese = "35"
              open output tivaese
           end-if.
           close tivaese.

           open input timposte.
           if status-timposte = "35"
              open output timposte
           end-if.
           close timposte.

           open input articoli.
           if status-articoli = "35"
              open output articoli
           end-if.            
           close articoli.

           open input assorcli.
           if status-assorcli = "35"
              open output assorcli
           end-if.
           close assorcli.

           open input progmag.
           if status-progmag = "35"
              open output progmag
           end-if.
           close progmag.

           open  input tordini.
           if status-tordini = "35"
              open output tordini
           end-if.
           close tordini.

           open input rordini.
           if status-rordini = "35"
              open output rordini
           end-if.
           close rordini.

      *****     open i-o rordini.
      *****     move 9999 to ror-anno.
      *****     start rordini key >= ror-chiave.
      *****     perform until 1 = 2
      *****        read rordini next at end exit perform end-read
      *****        delete rordini record
      *****     end-perform.
      *****     close rordini.

           open input eordini.
           if status-eordini = "35"
              open output eordini
           end-if.
           close eordini.

      *****     open  input int-tordini.
      *****     if status-int-tordini = "35"
      *****        open output int-tordini
      *****     end-if.
      *****     close int-tordini.         
      *****     
      *****     open input int-rordini.
      *****     if status-int-rordini = "35"
      *****        open output int-rordini
      *****     end-if.
      *****     close int-rordini.

      *****     open  input btordini.
      *****     if status-btordini = "35"
      *****        open output btordini
      *****     end-if.
      *****     close btordini.
      *****
      *****     open input brordini.
      *****     if status-brordini = "35"
      *****        open output brordini
      *****     end-if.
      *****     close brordini.

           open input tnotacr.
           if status-tnotacr = "35"
              open output tnotacr
           end-if.
           close tnotacr.

           open input rnotacr.
           if status-rnotacr = "35"
              open output rnotacr
           end-if.
           close rnotacr. 

           open input btnotacr.
           if status-btnotacr = "35"
              open output btnotacr
           end-if.
           close btnotacr.

           open input brnotacr.
           if status-brnotacr = "35"
              open output brnotacr
           end-if.
           close brnotacr.

           open input tcontat.
           if status-tcontat = "35"
              open output tcontat
           end-if.
           close tcontat.

           open input tcaumag.
           if status-tcaumag = "35"
              open output tcaumag
           end-if.
           close tcaumag.

           open input tmovmag.
           if status-tmovmag = "35"
              open output tmovmag
           end-if.
           close tmovmag.

           open input rmovmag.
           if status-rmovmag = "35"
              open output rmovmag
           end-if.
           close rmovmag.

           open input timbalqta.
           if status-timbalqta = "35"
              open output timbalqta
           end-if.
           close timbalqta.

           open input tparamge.
           if status-tparamge = "35"
              open output tparamge
           end-if.
           close tparamge. 

           open input tparamge2.
           if status-tparamge2 = "35"
              open output tparamge2
           end-if.
           close tparamge2. 

           open input distinteb.
           if status-distinteb = "35"
              open output distinteb
           end-if.
           close distinteb. 

           open input tcaudisb.
           if status-tcaudisb = "35"
              open output tcaudisb
           end-if.
           close tcaudisb.  

           open input statraff.
           if status-statraff = "35"
              open output statraff
           end-if.
           close statraff.  

           open input statsett.
           if status-statsett = "35"
              open output statsett
           end-if.
           close statsett.  

           open input giormag.
           if status-giormag = "35"
              open output giormag
           end-if.
           close giormag.

           open input tgiormag.
           if status-tgiormag = "35"
              open output tgiormag
           end-if.
           close tgiormag.

           open input ordfor.
           if status-ordfor = "35"
              open output ordfor
           end-if.
           close ordfor.

           open input ordfor2.
           if status-ordfor2 = "35"
              open output ordfor2
           end-if.
           close ordfor2.   

           open input ordfor-old.
           if status-ordfor-old = "35"
              open output ordfor-old
           end-if.
           close ordfor-old.

           open input movutf.
           if status-movutf = "35"
              open output movutf
           end-if.
           close movutf.  

           open input anautf.
           if status-anautf = "35"
              open output anautf
           end-if.
           close anautf.  

           open input tmovtrat.
           if status-tmovtrat = "35"
              open output tmovtrat
           end-if.
           close tmovtrat.

           open input lisagente.
           if status-lisagente = "35"
              open output lisagente
           end-if.
           close lisagente.

           open input provvig.
           if status-provvig = "35"
              open output provvig
           end-if.
           close provvig.  

           open input tarifvet.
           if status-tarifvet = "35"
              open output tarifvet
           end-if.
           close tarifvet. 

           open input trasporti.
           if status-trasporti = "35"
              open output trasporti
           end-if.
           close trasporti. 

           open input statmese.
           if status-statmese = "35"
              open output statmese
           end-if.
           close statmese. 

           open input G2.
           if status-G2 = "35"
              open output G2
           end-if.
           close G2.

           open input tconvanno.
           if status-tconvanno = "35"
              open output tconvanno
           end-if.

           close tconvanno.

           open input lockfile.
           if status-lockfile = "35"
              open output lockfile
           end-if.
           close lockfile. 

           open input lockname.
           if status-lockname = "35"
              open output lockname
           end-if.
           close lockname. 

           open input logfile.
           if status-logfile = "35"
              open output logfile
           end-if.
           close logfile.  

           open input tpiombo.
           if status-tpiombo = "35"
              open output tpiombo
           end-if.
           close tpiombo.  

           open input vettel.
           if status-vettel = "35"
              open output vettel
           end-if.
           close vettel.

           open input tescons.
           if status-tescons = "35"
              open output tescons
           end-if.
           close tescons.

           open input tesconsvet.
           if status-tesconsvet = "35"
              open output tesconsvet
           end-if.
           close tesconsvet.

           open input tagli.
           if status-tagli = "35"
              open output tagli
           end-if.
           close tagli.    

           open input listini.
           if status-listini = "35"
              open output listini
           end-if.                      
      *     move low-value to lst-rec.
      *     move 20210101  to lst-data.  
      *     start listini key >= lst-k-data
      *           invalid continue
      *       not invalid
      *           perform until 1 = 2
      *              read listini next at end exit perform end-read
      *              if lst-data < 20210101
      *                 exit perform cycle
      *              end-if               
      *              add 1 to nn
      *           end-perform
      *     end-start.
      *     display message nn.
           close listini.  

           open input tpromo.
           if status-tpromo = "35"
              open output tpromo
           end-if.
           close tpromo.   

           open input rpromo.
           if status-rpromo = "35"
              open output rpromo
           end-if.
           close rpromo.     

           open input usr-tel.
           if status-usr-tel = "35"
              open output usr-tel
           end-if.
           close usr-tel.

           open input locali.
           if status-locali = "35"
              open output locali
           end-if.
           close locali. 

           open input blister.
           if status-blister = "35"
              open output blister
           end-if.
           close blister.

           open input progmagric.
           if status-progmagric = "35"
              open output progmagric
           end-if.
           close progmagric.

           open input contestazioni.
           if status-contestazioni = "35"
              open output contestazioni
           end-if.
           close contestazioni.

           open input note-cont.
           if status-note-cont = "35"
              open output note-cont
           end-if.
           close note-cont.

           open input tscorte.
           if status-tscorte = "35"
              open output tscorte
           end-if.
           close tscorte.

           open input tlistini.
           if status-tlistini = "35"
              open output tlistini
           end-if.
           close tlistini.

           open input rlistini.     
           if status-rlistini = "35"
              open output rlistini
           end-if.
           open input tlistini
           move low-value to rlis-rec.
           start rlistini key >= rlis-chiave
           perform until 1 = 2
              read rlistini next at end exit perform end-read
              move rlis-codice to tlis-codice
              read tlistini no lock
              if rlis-ini-val  not = tlis-ini-val or
                 rlis-fine-val not = tlis-fine-val 
                 display message tlis-codice
              end-if
           end-perform

           close tlistini.
           close rlistini.

           open input nforn.
           if status-nforn = "35"
              open output nforn
           end-if.
           close nforn.

           open input nforn-dest.
           if status-nforn-dest = "35"
              open output nforn-dest
           end-if.
           close nforn-dest.

           open input ttipoavv.
           if status-ttipoavv = "35"
              open output ttipoavv
           end-if.
           close ttipoavv.

           open input useravv.
           if status-useravv = "35"
              open output useravv
           end-if.
           close useravv.

           open input tordforn.
           if status-tordforn = "35"
              open output tordforn
           end-if.
           close tordforn. 

           open input rordforn.
           if status-rordforn = "35"
              open output rordforn
           end-if.
           close rordforn.

           open input sordforn.
           if status-sordforn = "35"
              open output sordforn
           end-if.
           close sordforn.

           open input catart.
           if status-catart = "35"
              open output catart
           end-if.
           close catart.

           open input promoeva.
           if status-promoeva = "35"
              open output promoeva
           end-if.
           close promoeva.

           open input tparameva.
           if status-tparameva = "35"
              open output tparameva
           end-if.
           close tparameva.

           open input paramshi.
           if status-paramshi = "35"
              open output paramshi
           end-if.
           close paramshi.

           open input paramget.
           if status-paramget = "35"
              open output paramget
           end-if.
           close paramget.

           open input cli-prg.
           if status-cli-prg = "35"
              open output cli-prg
           end-if.
           close cli-prg. 

           open input contab.
           if status-contab = "35"
              open output contab
           end-if.
           close contab.     

           open input pagbloc.
           if status-pagbloc = "35"
              open output pagbloc
           end-if.
           close pagbloc.    

           open input tgruppi.
           if status-tgruppi = "35"
              open output tgruppi
           end-if.
           close tgruppi.

           open input tsetinvio.
           if status-tsetinvio = "35"
              open output tsetinvio
           end-if.
           close tsetinvio.

      *****     open input evaclides.
      *****     if status-evaclides = "35"
      *****        open output evaclides
      *****     end-if.
      *****     close evaclides.

      *****     open input progmag.
      *****     open input rordforn.
      *****     move low-value to rof-rec.
      *****     start rordforn key >= rof-chiave.
      *****     perform until 1 = 2
      *****        read rordforn next at end exit perform end-read
      *****        move rof-prg-chiave to prg-chiave
      *****        read progmag 
      *****             invalid display message rof-chiave
      *****        end-read
      *****     end-perform.
      *****     close progmag.

           open input reltor.
           if status-reltor = "35"
              open output reltor
           end-if.
           close reltor.  

           open input coperfab.
           if status-coperfab = "35"
              open output coperfab
           end-if.
           close coperfab.

           open input multigest.
           if status-multigest = "35"
              open output multigest
              move spaces to mul-rec
              write mul-rec
           end-if.
           close multigest.

           open input teva.
           if status-teva = "35"
              open output teva
           end-if.
           close teva.

           open input reva.
           if status-reva = "35"
              open output reva
           end-if.
           close reva.

           open input art-ordforn.
           if status-art-ordforn = "35"
              open output art-ordforn
           end-if.
           close art-ordforn.

           open input notef.
           if status-notef = "35"
              open output notef
           end-if.
           close notef.

           open input qta-vend.
           if status-qta-vend = "35"
              open output qta-vend
           end-if.
           close qta-vend.

           open input mtordini.
           if status-mtordini = "35"
              open output mtordini
           end-if.
           close mtordini.

           open input mrordini.     
           if status-mrordini = "35"
              open output mrordini
           end-if.
           close mrordini.

           open input impforn.
           if status-impforn = "35"
              open output impforn
           end-if.
           close impforn. 

           open input sitfin.
           if status-sitfin = "35"
              open output sitfin
           end-if.
           close sitfin.  

           open input param.
           if status-param = "35"
              open output param
           end-if.
           close param.  

           open input nlistini.
           if status-nlistini = "35"
              open output nlistini
           end-if.
           close nlistini.

           open input art-exp-shi.
           if status-art-exp-shi = "35"
              open output art-exp-shi
           end-if.
           close art-exp-shi. 

           open input pgmexe.
           if status-pgmexe = "35"
              open output pgmexe
           end-if.
           close pgmexe. 

           open input batnott.
           if status-batnott = "35"
              open output batnott
           end-if.
           close batnott. 

           open input stato-invio.
           if status-stato-invio = "35"
              open output stato-invio
           end-if.
           close stato-invio.

           open input qta-pordini.
           if status-qta-pordini = "35"
              open output qta-pordini
           end-if.
           close qta-pordini.

           open input paramfel.
           if status-paramfel = "35"
              open output paramfel
           end-if.
           close paramfel.

           open input tnumordf.
           if status-tnumordf = "35"
              open output tnumordf
           end-if.
           close tnumordf.

           open input grade.
           if status-grade = "35"
              open output grade
           end-if.
           close grade.

           open input battsost.
           if status-battsost = "35"
              open output battsost
           end-if.
           close battsost.  

           open input macrobatch.
           if status-macrobatch = "35"
              open output macrobatch
           end-if.
           close macrobatch.

           open input hleb.
           if status-hleb = "35"
              open output hleb
              initialize hleb-rec replacing numeric data by zeroes
                                       alphanumeric data by spaces
              write hleb-rec
           end-if.
           close hleb.      

           open input anacap.
           if status-anacap = "35"
              open output anacap
           end-if.
           close anacap.

           open input lock-div.
           if status-lock-div = "35"
              open output lock-div
           end-if.
           close lock-div.

           open input log4mas.
           if status-log4mas = "35"
              open output log4mas
           end-if.
           close log4mas.

           copy "EDI-procedure.cpy".

           goback.
