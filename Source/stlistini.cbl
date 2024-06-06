       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      stlistini.
       AUTHOR.                          Luciano.
      ******************************************************************

       SPECIAL-NAMES. 
           decimal-point is comma.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "tlistini.sl".
           copy "rlistini.sl".
           copy "nlistini.sl".
           copy "clienti.sl".
           copy "destinif.sl".
           copy "lineseq.sl".
           copy "articoli.sl".
           copy "impforn.sl".
           copy "timposte.sl".
           copy "tpiombo.sl".
           copy "tparamge.sl".
           copy "distinteb.sl".
           copy "progmag.sl".
           copy "param.sl".
           copy "tscorte.sl".  
           copy "tmagaz.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "tlistini.fd".
           copy "rlistini.fd".
           copy "nlistini.fd".
           copy "clienti.fd".
           copy "destinif.fd".
           copy "lineseq.fd".
           copy "articoli.fd".
           copy "impforn.fd".
           copy "timposte.fd".
           copy "tpiombo.fd". 
           copy "tparamge.fd".
           copy "distinteb.fd".
           copy "progmag.fd".
           copy "param.fd".
           copy "tscorte.fd". 
           copy "tmagaz.fd".

       WORKING-STORAGE SECTION. 
           copy "acucobol.def".
           copy "acugui.def".
           copy "spooler.def".
           copy "fonts.def".
           copy "selprint.lks".
           copy "common-excel.def".
           copy "link-geslock.def".
           copy "prz-finito-forn.def".
           copy "imposte-fornitore.def".
           copy "costo-medio.def".
           copy "trova-parametro.def".

       77  como-ora pic 9(8).

       78  titolo             value "Stampa Listini".

       77  status-tlistini    pic xx.
       77  status-rlistini    pic xx.
       77  status-nlistini    pic xx.
       77  status-clienti     pic xx.
       77  status-destinif    pic xx.
       77  status-lineseq     pic xx.
       77  status-articoli    pic xx.
       77  status-impforn     pic xx.
       77  status-timposte    pic xx.
       77  status-tpiombo     pic xx.
       77  status-tparamge    pic xx.
       77  status-distinteb   pic xx.
       77  status-progmag     pic xx.  
       77  status-param       pic xx.   
       77  status-tscorte     pic xx.
       77  status-tmagaz      pic xx.

       77  dir-Handle         handle.
       77  idx                pic 9(5).
       77  wstampa            pic x(256).
       77  ed-stlis-codice    pic z(15).

       77  messaggio          pic X(150) value spaces.
       77  font-size-dply     pic Z(5).
       77  WFONT-STATUS       pic s9(5) value 0.

       77  como-data          pic 9(8).
       01  riga-listino.
           10 rl-articolo             PIC  9(6).
           10 rl-descrizione          pic  x(50).
           10 rl-art-forn             PIC  x(20).
           10 rl-prz-acq              PIC  9(9),9(4).
           10 rl-sconto-1             PIC  9(3),9(2).
           10 rl-sconto-2             PIC  9(3),9(2).
           10 rl-sconto-3             PIC  9(3),9(2).
           10 rl-sconto-4             PIC  9(3),9(2).
           10 rl-sconto-5             PIC  9(3),9(2).
           10 rl-costi-agg            PIC  9(9),9(4).
           10 rl-perce-agg            PIC  9(3),9(2).
           10 rl-disponibilita        PIC  9(9).
           10 rl-lead-time            PIC  9(3).
           10 rl-tipo-tratt-imposte   PIC  9(5).
           10 rl-imposte-descr        PIC  x(50).
           10 rl-piombo               PIC  9(3),9(3).
           10 rl-netto                PIC  9(9),9(4).
           10 rl-PFA                  pic  x.
           10 rl-prz-confronto        PIC  9(6),9999.
           10 rl-prz-reale            PIC  9(6),9999.
           10 rl-premi-fine-anno      PIC  9(3),9(2).

       01  riga-listino-stampa.
           10 rls-articolo            PIC  z(5)9.
           10 filler                  pic x value space.
           10 rls-descrizione         pic  x(25).
           10 filler                  pic x value space.
           10 rls-art-forn            PIC  x(20).
           10 filler                  pic x value space.
           10 rls-prz-acq             PIC  zzz.zzz.zz9,9(4).
           10 filler                  pic x value space.
           10 rls-sconto-1            PIC  zz9,9(2).
           10 filler                  pic x value space.
           10 rls-sconto-2            PIC  zz9,9(2).
           10 filler                  pic x value space.
           10 rls-sconto-3            PIC  zz9,9(2).
           10 filler                  pic x value space.
           10 rls-sconto-4            PIC  zz9,9(2).
           10 filler                  pic x value space.
           10 Rls-Sconto-5            PIC  zz9,9(2).
           10 filler                  pic x value space.
           10 rls-netto               PIC  zzz.zzz.zz9,9(4).
           10 filler                  pic x value space.
           10 rls-costi-agg           PIC  z.zzz.zz9,9(4).
           10 filler                  pic x value space.
           10 rls-perce-agg           PIC  zz9,9(2).
           10 filler                  pic x value space.
           10 rls-piombo              PIC  zz9,9(2).
           10 filler                  pic x value space.
           10 rls-disponibilita       PIC  zzz.zzz.zz9.
           10 filler                  pic x value space.
           10 rls-lead-time           PIC  z(3).
           10 filler                  pic x value space.
           10 rls-tipo-tratt-imposte  PIC  z(5).
           10 filler                  pic x value space.
           10 rls-imposte-descr       PIC  x(17).
           10 filler                  pic x value space.
           10 filler                  pic x value space.
           10 rls-PFA                 PIC  x.
           10 filler                  pic x value space.
           10 filler                  pic x value space.
           10 rls-prz-reale           PIC  zzz.zz9,9(4).
           10 filler                  pic x value space.
           10 rls-prz-confronto       PIC  zzz.zz9,9(4).

       01  riga-listino-testa.
           10 filler                  PIC x(6) value "  Art.".
           10 filler                  pic x value space.
           10 filler                  pic x(25) value "Descrizione".
           10 filler                  pic x value space.
           10 filler                  PIC x(20) value "Cod. Fornitore".
           10 filler                  pic x value space.
           10 filler               PIC x(16) value "   Prz. Acquisto".
           10 filler                  pic x value space.
           10 filler                  PIC x(6) value "% sc.1".
           10 filler                  pic x value space.
           10 filler                  PIC x(6) value "% sc.2".
           10 filler                  pic x value space.
           10 filler                  PIC x(6) value "% sc.3".
           10 filler                  pic x value space.
           10 filler                  PIC x(6) value "% sc.4".
           10 filler                  pic x value space.
           10 filler                  PIC x(6) value "% sc.5".
           10 filler                  pic x value space.
           10 filler               PIC x(16) value "           Netto".
           10 filler                  pic x value space.
           10 filler               PIC x(14) value "    Costi Agg.".
           10 filler                  pic x value space.
           10 filler                  PIC x(6) value "% Agg.".
           10 filler                  pic x value space.
           10 filler                  PIC x(6) value " % Pb.".
           10 filler                  pic x value space.
           10 filler                  PIC x(11) value "      Pezzi".
           10 filler                  pic x value space.
           10 filler                  PIC x(3) value " LT".
           10 filler                  pic x value space.
           10 filler                  PIC x(23) value "Imposta".
           10 filler                  pic x value space.
           10 filler                  PIC x(3) value "PFA".
           10 filler                  pic x value space.
           10 filler                  PIC x(12) value "  Prz. reale".
           10 filler                  pic x value space.
           10 filler                  PIC x(12) value "   Confronto".

       77  Arial14BI     handle of font.
       77  arial5B       handle of font.
       77  arial5        handle of font.
       77  arial6        handle of font.
       77  courier       handle of font.


       77  MaxRighe             pic 9(3).|
       78  MaxRighe-con-note    value 33.
       78  MaxRighe-senza-note  value 40.

       77  passo              pic 99v99.
  
       01  filler             pic 9.
         88 Prima-Volta       value 1, false 0.

       01  filler             pic 9.
         88 prima-nota        value 1, false 0.


         
       01  controlli          pic xx.
         88 tutto-ok          value "OK".
         88 errori            value "ER".


       77  num-pagina           pic 9(3)    value zero.
       77  num-pag-ed           pic z(3)    value zero.
       77  riga                 pic 99.
       77  pronostico           pic 99.
       77  logo-handle          handle of bitmap.
       77  user-codi            pic x(10).

       77  cons-ragsoc    pic x(40).
       77  cons-ind       pic x(40).
       77  cons-localita  pic x(40).
       77  cons-piva      pic x(40).  
       77  calcolo-piombo        pic x.
         88 nuovo-calcolo-piombo value "N".

       LINKAGE SECTION.
           copy "link-stlistini.def".

      ******************************************************************
       PROCEDURE DIVISION using stlistini-linkage.

       DECLARATIVES.
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
           when "93"
                initialize geslock-messaggio
                string   "Chiudere file Excel!"
                  x"0d0a""Impossibile procedere!" delimited size
                      into geslock-messaggio
                end-string
                move 1 to geslock-v-riprova
                move 0 to geslock-v-ignora
                move 1 to geslock-v-termina
                move   "File CSV"   to geslock-nome-file
                call   "geslock" using geslock-linkage
                cancel "geslock"
                evaluate true
                when riprova
                     open output lineseq
                when termina
                     set errori to true
                     display message "Operazione interrotta!"
                               title titolo
                                icon 2
                end-evaluate
           end-evaluate.
       END DECLARATIVES.


      ***--- 
       MAIN-PRG.
           perform INIT.
           if tutto-ok
              perform OPEN-FILES
              perform ELABORAZIONE
              perform CLOSE-FILES
           end-if
           perform EXIT-PGM.

      ***---
       INIT.
           move stlis-user to user-codi.
           accept como-data from century-date
           accept como-ora  from time
       
           COPY RESOURCE "logo-st.bmp".

           set tutto-ok   to true.
           set Prima-Volta to true.

           evaluate true
           when stlis-excell
                accept como-data   from century-date
                perform ACCETTA-SEPARATORE
                accept wstampa from environment "PATH_CSV_LISTINI"
                if wstampa = space
                   display message box "Variabile d'ambiente PATH_CSV_LI
      -                                "STINI non valorizzata"
                          title titolo
                   set errori  to true
                end-if
                perform CONTROLLA-CARTELLA
                inspect wstampa
                                replacing trailing space by low-value
                move stlis-codice  to ed-stlis-codice
                call "C$JUSTIFY" using ed-stlis-codice, "L"
                inspect ed-stlis-codice  
                                   replacing trailing space by low-value
                string wstampa           delimited by low-value
                       "Listino_"        delimited by size
                       ed-stlis-codice   delimited by low-value
                       "_"               delimited by size
                       como-data         delimited by size
                       ".csv"            delimited by size
                       into wstampa
                inspect wstampa
                                replacing trailing low-value by space
           when stlis-printer
           when stlis-anteprima
                set environment "PRINTER" to "-P SPOOLER"
                CALL "w$bitmap" USING WBITMAP-LOAD "logo-st.BMP", 
                      GIVING logo-handle

           end-evaluate.


      ***---
       OPEN-FILES.
           open input tlistini  
           open input rlistini  
           open input nlistini  
           open input clienti.
           open input destinif.
           open input articoli.
           open input impforn.
           open input timposte.
           open input tpiombo.
           open input tparamge.
           open input distinteb.
           open input progmag.
           open input param.
           open input tscorte.
           open input tmagaz.

      ***---
       ELABORAZIONE.
           move stlis-lis-chiave to tlis-chiave.
           read tlistini no lock invalid continue end-read.
           move tlis-fornitore  to cli-codice.
           set cli-tipo-F to true.
           read clienti invalid continue end-read.
      
           move tlis-fornitore   to desf-codice
           move tlis-destino     to desf-prog  
           read destinif invalid continue end-read.

           evaluate true
           when stlis-excell
                open output LINESEQ
                if errori
                   display message box
                                   "impossibile creare il file CSV"
                        title = titolo
                        icon 2
                else
                   perform INTESTA-CSV
                   perform STAMPA-RIGHE
                   close LINESEQ
                   perform CALL-EXCEL-2
                end-if
           when stlis-printer
           when stlis-anteprima
                move 90   to riga
                perform STAMPA-RIGHE
                if not prima-volta
                   set spl-chiusura to true
                   call   "spooler" using spooler-link
                   cancel "spooler"
                end-if
           end-evaluate.

      ***---
       INTESTA-CSV.
           initialize line-riga.
           string "Articolo Lubex"          delimited by size
                  separatore                delimited by size
                  "Descrizione"             delimited by size
                  separatore                delimited by size
                  "Articolo Fornitore"      delimited by size
                  separatore                delimited by size
                  "Prz. Acquisto"           delimited by size
                  separatore                delimited by size
                  "% sc. 1"                 delimited by size
                  separatore                delimited by size
                  "% sc. 2"                 delimited by size
                  separatore                delimited by size
                  "% sc. 3"                 delimited by size
                  separatore                delimited by size
                  "% sc. 4"                 delimited by size
                  separatore                delimited by size
                  "% sc. 5"                 delimited by size
                  separatore                delimited by size
                  "Netto"                   delimited by size
                  separatore                delimited by size
                  "Costi Agg."              delimited by size
                  separatore                delimited by size
                  "% Agg."                  delimited by size
                  separatore                delimited by size
                  "% Add. Pb"               delimited by size
                  separatore                delimited by size
                  "Disponibilità"           delimited by size
                  separatore                delimited by size
                  "Lead-Time"               delimited by size
                  separatore                delimited by size
                  "Tipologia Imposte"       delimited by size         
                  separatore                delimited by size
                  "Descrizione"             delimited by size         
                  separatore                delimited by size
                  "PFA"                     delimited by size
                  separatore                delimited by size
                  "Prezzo Reale"            delimited by size
                  separatore                delimited by size
                  "Confronto"               delimited by size
                  separatore                delimited by size
                  "Premio Fornitore"        delimited by size
                  into line-riga
           write line-riga.


      ***---
       STAMPA-INTESTAZIONE.
           add 1 to num-pagina

           move 0        to riga.

           if num-pagina = 1
              perform STAMPA-NOTE
           end-if

           perform FINCATURA.

           perform RIQUADRO-FORNITORE.

           perform RIQUADRO-DESTINO.

           set spl-stringa   to true.
           move arial6       to spl-hfont.
           move 2,2          to spl-riga.
           move 17 to spl-colonna

           move tlis-descrizione   to spl-riga-stampa
           call "spooler" using spooler-link.

           add 0,3  to spl-riga
           initialize spl-riga-stampa
           string "Inizio validità "  delimited by size
                  tlis-ini-val(7:2)   delimited by size
                  "/"                 delimited by size
                  tlis-ini-val(5:2)   delimited by size
                  "/"                 delimited by size
                  tlis-ini-val(1:4)   delimited by size
                  " Fine validità "   delimited by size
                  tlis-fine-val(7:2)  delimited by size
                  "/"                 delimited by size
                  tlis-fine-val(5:2)  delimited by size
                  "/"                 delimited by size
                  tlis-fine-val(1:4)  delimited by size
                  into spl-riga-stampa
           call "spooler" using spooler-link.

           add 0,3  to spl-riga

           evaluate true
           when tlis-trasp-f-incluso
                 move "Trasporto fornitore incluso"  to spl-riga-stampa
           when tlis-trasp-f-escluso 
                 move "Trasporto fornitore escluso"  to spl-riga-stampa
           end-evaluate.
           call "spooler" using spooler-link.
                                    
           add 0,3  to spl-riga

           evaluate true
           when tlis-trasp-c-incluso
                 move "Trasporto a cliente incluso"  to spl-riga-stampa
           when tlis-trasp-c-escluso 
                 move "Trasporto a ccliente escluso" to spl-riga-stampa
           end-evaluate.
           call "spooler" using spooler-link.

           if tlis-mag not = spaces   
              move tlis-mag to mag-codice
              read tmagaz no lock invalid continue end-read
              add 0,33 to spl-riga
              initialize spl-riga-stampa
              string "MAGAZZINO: " delimited size
                     tlis-mag      delimited size
                     " - "         delimited size
                     mag-descrizione
                into spl-riga-stampa
              end-string
              call "spooler" using spooler-link
           end-if.

           move 18,9  to spl-riga

           move arial6       to spl-hfont.
           move 27,3  to spl-colonna
      *     move 0,6  to spl-colonna
           initialize spl-riga-stampa
           move num-pagina   to num-pag-ed
           string "PAGINA "  delimited by size
                  num-pag-ed delimited by size
                  into spl-riga-stampa
           call "spooler" using spooler-link.

      *    posizionamento per partenza righe
           move 4,15 to spl-riga.
           move courier   to spl-hfont.
           move 0,5  to spl-colonna
           move zero   to spl-tipo-colonna
           move riga-listino-testa to spl-riga-stampa.
           call "spooler" using spooler-link.

           move 4,3 to spl-riga.

           if num-pagina = 1
              if prima-nota
                 move MaxRighe-senza-note   to MaxRighe
              else  
                 move MaxRighe-con-note     to MaxRighe
              end-if
           else
              move MaxRighe-senza-note      to MaxRighe
           end-if.

      ***---
       RIQUADRO-FORNITORE.
           move 8     to spl-pen-with.
           move 2,1  to spl-riga.
           add 1,7  to spl-riga giving spl-riga-fine

           move 0,9                to spl-colonna.
           add 7,0 to spl-colonna giving spl-colonna-fine.

           set  spl-oggetto           to true.
           set  SPL-RETTANGOLO-ROUND  to true.
           set  spl-brush-null        to true.
           set  spl-nero              to true.
           call "spooler"         using spooler-link.

           set  SPL-linea to true.
           move 4         to spl-pen-with.
           add 0,1        to spl-colonna
           subtract 0,1   from spl-colonna-fine
      
      *    righe interne
           add 0,3        to spl-riga
           perform 4 times
              add 0,3        to spl-riga
              move spl-riga  to spl-riga-fine
              call "spooler"         using spooler-link
           end-perform.


           set spl-stringa   to true.
           move arial6       to spl-hfont.
           subtract 1,45      from spl-riga.
      *     subtract 0,6      from spl-colonna.
           move "FORNITORE"  to spl-riga-stampa
           call "spooler" using spooler-link.


           set cli-tipo-F       to true
           move tlis-fornitore  to cli-codice
           read clienti
              invalid
                 continue
           end-read

           move cli-ragsoc-1 to cons-ragsoc
           move cli-indirizzo   to cons-ind
           initialize cons-localita
           inspect cli-localita replacing trailing space by low-value
           string cli-cap       delimited by size
                  " "           delimited by size
                  cli-localita  delimited by low-value
                  " ("          delimited by size
                  cli-prov      delimited by size
                  ") "          delimited by size
                  cli-nazione   delimited by size
                  into cons-localita
           initialize cons-piva
           if cli-piva not = zero
              string "P.IVA "      delimited by size
                     cli-piva      delimited by size
                     into cons-piva
           end-if


           add 0,3  to spl-riga
           move cons-ragsoc     to spl-riga-stampa
           call "spooler" using spooler-link.
           add 0,3  to spl-riga
           move cons-ind        to spl-riga-stampa
           call "spooler" using spooler-link.
           add 0,3  to spl-riga
           move cons-localita   to spl-riga-stampa
           call "spooler" using spooler-link.

           add 0,3  to spl-riga
           move cons-piva       to spl-riga-stampa
           call "spooler" using spooler-link.

      ***---
       RIQUADRO-DESTINO.
           move 8     to spl-pen-with.
           move 2,1  to spl-riga.
           add 1,7  to spl-riga giving spl-riga-fine

           move 8,5                to spl-colonna.
           add 7,0 to spl-colonna giving spl-colonna-fine.

           set  spl-oggetto        to true.
           set  SPL-RETTANGOLO-ROUND  to true.
           set  spl-brush-null     to true.
           set  spl-nero           to true.
           call "spooler"         using spooler-link.

           set  SPL-linea to true.
           move 4         to spl-pen-with.
           add 0,1        to spl-colonna
           subtract 0,1   from spl-colonna-fine
      
      *    righe interne
           add 0,3        to spl-riga
           perform 4 times
              add 0,3        to spl-riga
              move spl-riga  to spl-riga-fine
              call "spooler"         using spooler-link
           end-perform.

           set spl-stringa   to true.
           move arial6       to spl-hfont.
           subtract 1,45      from spl-riga.
           move "DESTINO"  to spl-riga-stampa
           call "spooler" using spooler-link.


           move tlis-fornitore  to desf-codice
           move tlis-destino    to desf-prog

           read destinif
              invalid
                 continue
           end-read

           move desf-ragsoc-1 to cons-ragsoc
           move desf-indirizzo   to cons-ind
           initialize cons-localita
           inspect desf-localita replacing trailing space by low-value
           string desf-cap       delimited by size
                  " "           delimited by size
                  desf-localita  delimited by low-value
                  " ("          delimited by size
                  desf-prov      delimited by size
                  ") "          delimited by size
                  desf-nazione   delimited by size
                  into cons-localita
 
           add 0,3  to spl-riga
           move cons-ragsoc     to spl-riga-stampa
           call "spooler" using spooler-link.
           add 0,3  to spl-riga
           move cons-ind        to spl-riga-stampa
           call "spooler" using spooler-link.
           add 0,3  to spl-riga
           move cons-localita   to spl-riga-stampa
           call "spooler" using spooler-link.


      ***---
       FINCATURA.
      *    dati Lubex
           perform DATI-LUBEX.

      *    logo 
           move logo-handle to spl-hbitmap
           set  spl-bitmap  to true
           move 2,0 to spl-riga
           move 6,5 to spl-colonna
           move 1,2 to spl-bitmap-height
           move 2,8 to spl-bitmap-width
           call "spooler" using spooler-link.

      *    riga Ordine
           set spl-stringa            to true.
           move Arial14BI             to spl-hfont.
           move 1,4                   to spl-riga.
           move 17,0                  to spl-colonna.
           move "OFFERTE FORNITORE"   to spl-riga-stampa
           call "spooler" using spooler-link.

           set spl-nero   to true.
           move 20                 to spl-pen-with.
           move 0,6                to spl-colonna.
           move 16,5               to spl-colonna-fine.
           move 1,8   to spl-riga 
                         spl-riga-fine.
           set  spl-oggetto        to true.
           set  spl-linea          to true.
           set  spl-pen-solid      to true.
           call "spooler"       using spooler-link.

           move 22,8                to spl-colonna.
           move 28,5               to spl-colonna-fine.
           call "spooler"       using spooler-link.

           add 2,2   to spl-riga 
           move spl-riga  to spl-riga-fine.
           move 0,6                to spl-colonna.
           move 28,5               to spl-colonna-fine.
           call "spooler"       using spooler-link.


           add 14,8       to spl-riga 
           move 28,5      to spl-colonna-fine.
           move spl-riga  to spl-riga-fine.
quii       call "spooler"       using spooler-link.

           move 4         to spl-pen-with.
           move 4,4       to spl-riga 
           move spl-riga  to spl-riga-fine.
quii       call "spooler"       using spooler-link.

      ***---
       STAMPA-RIGHE.
           move tlis-chiave  to  rlis-codice.
           move low-value    to  rlis-articolo.
           start rlistini key >= rlis-k-descr of rlistini
              invalid 
                 continue
              not invalid
                 perform until 1 = 2
                    read rlistini next 
                       at end 
                          exit perform 
                    end-read
                    if tlis-chiave not = rlis-codice
                       exit perform
                    end-if
                    evaluate true
                    when stlis-excell
                         perform SCRIVI-EXCEL
                    when stlis-printer
                    when stlis-anteprima
                         perform SCRIVI
                         if prima-volta
                            exit perform
                         end-if
                    end-evaluate
                 end-perform
           end-start.

      *     if not prima-volta
      *        evaluate true
      *        when stlis-printer
      *        when stlis-anteprima
      *             perform STAMPA-NOTE
      *        end-evaluate
      *     end-if.

      ***---
       SCRIVI-EXCEL.
           move rlis-articolo   to rl-articolo
                                   art-codice
           if rlis-des-libera not = space
              move rlis-des-libera to rl-descrizione
           else
              read articoli
                 invalid
                    initialize art-descrizione
              end-read
              move art-descrizione to rl-descrizione
           end-if

           move rlis-art-forn               to rl-art-forn      
           move rlis-prz-acq                to rl-prz-acq       
           move rlis-sconto-1               to rl-sconto-1      
           move rlis-sconto-2               to rl-sconto-2      
           move rlis-sconto-3               to rl-sconto-3      
           move rlis-sconto-4               to rl-sconto-4
           move rlis-sconto-5               to rl-sconto-5
           move rlis-costi-agg              to rl-costi-agg
           move rlis-perce-agg              to rl-perce-agg
           move rlis-disponibilita          to rl-disponibilita 
           move rlis-lead-time              to rl-lead-time     
           move rlis-tipo-tratt-imposte     to rl-tipo-tratt-imposte
                                               imf-codice
           read impforn no lock
              invalid
                 move space  to imf-descrizione
           end-read
           move imf-descrizione             to rl-imposte-descr

           move rlis-perce-pb               to rl-piombo.
           move rlis-netto                  to rl-netto.
           move rlis-PFA                    to rl-PFA.
                             
           move tlis-trasp-f to como-trasporto-f.
           move tlis-trasp-c to como-trasporto-c.
           move 0            to prg-peso-utf prg-peso-non-utf.
           perform CALCOLA-PRZ-FINITO.
           move prz-confronto               to rl-prz-confronto.
           move prz-reale                   to rl-prz-reale.

           move premio-FA  to rl-premi-fine-anno

           initialize line-riga.
           string rl-articolo            delimited by size
                  separatore             delimited by size
                  rl-descrizione         delimited by size
                  separatore             delimited by size
                  rl-art-forn            delimited by size 
                  separatore             delimited by size
                  rl-prz-acq             delimited by size 
                  separatore             delimited by size
                  rl-sconto-1            delimited by size 
                  separatore             delimited by size
                  rl-sconto-2            delimited by size 
                  separatore             delimited by size
                  rl-sconto-3            delimited by size 
                  separatore             delimited by size
                  rl-sconto-4            delimited by size 
                  separatore             delimited by size
                  rl-sconto-5            delimited by size 
                  separatore             delimited by size
                  rl-netto               delimited by size 
                  separatore             delimited by size
                  rl-costi-agg           delimited by size 
                  separatore             delimited by size
                  rl-perce-agg           delimited by size 
                  separatore             delimited by size
                  rl-piombo              delimited by size    
                  separatore             delimited by size
                  rl-disponibilita       delimited by size 
                  separatore             delimited by size
                  rl-lead-time           delimited by size
                  separatore             delimited by size
                  rl-tipo-tratt-imposte  delimited by size
                  separatore             delimited by size
                  rl-imposte-descr       delimited by size
                  separatore             delimited by size
                  rl-PFA                 delimited by size
                  separatore             delimited by size
                  rl-prz-reale           delimited by size
                  separatore             delimited by size
                  rl-prz-confronto       delimited by size
                  separatore             delimited by size
                  rl-premi-fine-anno     delimited by size
                  into line-riga
           write line-riga.

      ***---
       SCRIVI.
           add 1 to riga giving pronostico

           if pronostico > maxrighe
              perform SALTO-PAGINA
           end-if.
           if prima-volta
              exit paragraph
           end-if

           add 1 to riga.

           move rlis-articolo   to rls-articolo
                                   art-codice
           if rlis-des-libera not = space
              move rlis-des-libera to rls-descrizione
           else
              read articoli
                 invalid
                    initialize art-descrizione
              end-read
              move art-descrizione to rls-descrizione
           end-if.

           move rlis-art-forn               to rls-art-forn.
           move rlis-prz-acq                to rls-prz-acq.
           move rlis-sconto-1               to rls-sconto-1.
           move rlis-sconto-2               to rls-sconto-2.
           move rlis-sconto-3               to rls-sconto-3.
           move rlis-sconto-4               to rls-sconto-4.
           move rlis-sconto-5               to rls-sconto-5.
           move rlis-costi-agg              to rls-costi-agg.
           move rlis-perce-agg              to rls-perce-agg.
           move rlis-disponibilita          to rls-disponibilita.
           move rlis-lead-time              to rls-lead-time.
           move rlis-tipo-tratt-imposte     to rls-tipo-tratt-imposte
                                               imf-codice.
           read impforn no lock
                invalid move spaces to imf-descrizione
           end-read.
           move imf-descrizione             to rls-imposte-descr.

           move rlis-perce-pb               to rls-piombo.
           move rlis-netto                  to rls-netto.
           move rlis-PFA                    to rls-PFA.

           move tlis-trasp-f to como-trasporto-f.
           move tlis-trasp-c to como-trasporto-c.
           move 0            to prg-peso-utf prg-peso-non-utf.
           perform CALCOLA-PRZ-FINITO.

           move prz-confronto          to rls-prz-confronto
           move prz-reale              to rls-prz-reale

           move riga-listino-stampa    to spl-riga-stampa

           move zero    to spl-tipo-colonna
           add  0,35    to spl-riga.
           move 0,5     to spl-colonna
           move courier to spl-hfont

           set spl-stringa   to true.
           call "spooler" using spooler-link. 
           add passo to spl-riga.

      ***---
       CALCOLA-TRASPORTO.
           move 0 to costo-trasporto.
           move art-scorta to sco-codice.
           read tscorte no lock invalid continue end-read.

           move tlis-fornitore to desf-codice.
           move tlis-destino   to desf-prog.
           read destinif no lock invalid continue end-read.   
           if como-trasporto-f = 1
              compute costo-trasporto = 
                    ( art-peso-utf + art-peso-non-utf ) * sco-trasp-f
           end-if.
           if como-trasporto-c = 1
              compute costo-trasporto = costo-trasporto +
                    (( art-peso-utf + art-peso-non-utf ) * sco-trasp-c)
           end-if. 
           

      ***---
       SALTO-PAGINA.
           if Prima-Volta
              move 0 to passo
              initialize spooler-link

              move "Stampa Listini Fornitori" to spl-nome-job
      *    nuova selezione
              evaluate true 
              when stlis-printer
                   call   "selprint" using selprint-linkage
                   cancel "selprint"

              when stlis-anteprima
                   accept selprint-stampante
                                from environment "STAMPANTE_ANTEPRIMA"
                   move 1 to selprint-num-copie
              end-evaluate
              if selprint-stampante not = space
                 move selprint-num-copie to SPL-NUM-COPIE
                 move selprint-stampante to SPL-NOME-STAMPANTE
      *    NUOVA SELEZIONE
                 set spl-apertura to true
                 set SPL-HORIZONTAL to true
                 set WFDEVICE-WIN-PRINTER    to true
                 call "spooler" using spooler-link
                 set Prima-Volta   to false
                 if spl-sta-annu 
                    exit paragraph 
                 else
                    perform CARICA-FONT
                 end-if
      *    NUOVA SELEZIONE
              else
                 set spl-sta-annu to true

              end-if
              if spl-sta-annu
                 display message box
                                   "Procedura interrotta dall'utente"
                        title = titolo
                        icon 2
              else
                 set prima-volta   to false
              end-if
           else
              set spl-salto-pagina     to true
              call "spooler"        using spooler-link
              set spl-stringa to true
              move 0      to spl-riga
              move spaces to spl-riga-stampa
              call "spooler" using spooler-link
           end-if.

           if not prima-volta
              perform STAMPA-INTESTAZIONE
              move 0 to riga
           end-if.


      ***---
       CARICA-FONT.
           set tutto-ok             to true.
           initialize wfont-data.
           move 14                  to wfont-size.
           move "Arial"             to wfont-name.
           set wfont-bold           to true.
           set wfcharset-dont-care  to true    
           set wfont-italic         to true
           set wfont-underline      to false   
           set wfont-strikeout      to false   
           set wfont-fixed-pitch    to false   
           move zero                to wfont-char-set
           set wfdevice-win-printer to true.
           CALL "W$FONT" USING wfont-get-closest-font, 
                               Arial14BI, wfont-data
                               giving wfont-status.
      
           if wfont-status = wfonterr-font-not-found
              set errori to true
              perform MESSAGGIO-ERR-FONT
           end-if.

           initialize wfont-data.
           move 5                  to wfont-size.
           move "Arial"   to wfont-name.
           set wfont-bold           to true.
           set wfcharset-dont-care  to true    
           set wfont-italic         to false   
           set wfont-underline      to false   
           set wfont-strikeout      to false   
           set wfont-fixed-pitch    to false   
           move zero                to wfont-char-set
           set wfdevice-win-printer to true.
           CALL "W$FONT" USING wfont-get-closest-font, 
                               arial5B, wfont-data
                               giving wfont-status.
      
           if wfont-status = wfonterr-font-not-found
              set errori to true
              perform MESSAGGIO-ERR-FONT
           end-if.

           initialize wfont-data.
           move 5                  to wfont-size.
           move "Arial"   to wfont-name.
           set wfont-bold           to false.
           set wfcharset-dont-care  to true    
           set wfont-italic         to false   
           set wfont-underline      to false   
           set wfont-strikeout      to false   
           set wfont-fixed-pitch    to false   
           move zero                to wfont-char-set
           set wfdevice-win-printer to true.
           CALL "W$FONT" USING wfont-get-closest-font, 
                               arial5, wfont-data
                               giving wfont-status.
      
           if wfont-status = wfonterr-font-not-found
              set errori to true
              perform MESSAGGIO-ERR-FONT
           end-if.

           initialize wfont-data.
           move 6                  to wfont-size.
           move "Arial"   to wfont-name.
           set wfont-bold           to false.
           set wfcharset-dont-care  to true    
           set wfont-italic         to false   
           set wfont-underline      to false   
           set wfont-strikeout      to false   
           set wfont-fixed-pitch    to false   
           move zero                to wfont-char-set
           set wfdevice-win-printer to true.
           CALL "W$FONT" USING wfont-get-closest-font, 
                               arial6, wfont-data
                               giving wfont-status.
      
           if wfont-status = wfonterr-font-not-found
              set errori to true
              perform MESSAGGIO-ERR-FONT
           end-if.

           initialize wfont-data.
           move 6                   to wfont-size.
           move "Courier New"       to wfont-name.
           set wfont-bold           to false.
           set wfcharset-dont-care  to true    
           set wfont-italic         to false   
           set wfont-underline      to false   
           set wfont-strikeout      to false   
           set wfont-fixed-pitch    to false   
           move zero                to wfont-char-set
           set wfdevice-win-printer to true.
           CALL "W$FONT" USING wfont-get-closest-font, 
                               courier, wfont-data
                               giving wfont-status.
      
           if wfont-status = wfonterr-font-not-found
              set errori to true
              perform MESSAGGIO-ERR-FONT
           end-if.

      ***---
       MESSAGGIO-ERR-FONT.
      * ISACCO (MESSAGGIO DI ERRORE ED USCITA SE NON TROVA UN FONT)
           initialize messaggio.

           inspect WFONT-NAME replacing trailing space by low-value.
           move wfont-size    to font-size-dply.

           string  "Font: "         delimited size
                   wfont-name       delimited low-value
                   X"0D0A"          delimited size
                   "Dimensione: ",  delimited size
                   font-size-dply,  delimited size
                   X"0D0A"          delimited size
                   "Non installato. La stampa verrà abortita!"
                                    delimited size
              into messaggio.

           inspect messaggio replacing trailing space by low-value.

           display message messaggio.

      ***---
       CLOSE-FILES.
           close tlistini    
                 rlistini
                 nlistini
                 clienti
                 destinif
                 articoli
                 impforn
                 timposte
                 tpiombo
                 tparamge
                 distinteb
                 progmag
                 param
                 tscorte
                 tmagaz.

      ***---
       EXIT-PGM.
           set environment "PRINTER" to "-P SPOOLER-DIRECT".
           destroy arial14bi.
           destroy arial5B.
           destroy arial5.
           destroy arial6.
           destroy courier.
           goback.

      ***---
       CALL-EXCEL-2.
           accept path-to-subst from environment "PATH_CSV_LISTINI".
           accept path-to-excel from environment "PATH_CSV_LISTINI".
      *****     accept row-command   from environment "PR-EXCEL".
      *****     if row-command = null or spaces
      *****        perform FIND-REGISTRY-EXCEL
      *****     end-if.
           move "START" to row-command.
           if row-command not = spaces
              initialize cmd-lancio

              inspect row-command replacing trailing spaces
                                                  by low-value
              |Sostituisco il path locale col path di rete
              move 0 to cont
              move 0 to cont2

              inspect path-to-subst replacing trailing spaces 
                                                    by low-value
              inspect path-to-subst tallying cont  for characters 
                                                before low-value

              inspect path-to-excel replacing trailing spaces 
                                                    by low-value
              inspect path-to-excel tallying cont2 for characters 
                                                before low-value

              initialize path-launch
              move wstampa(cont + 1:)      to path-launch(cont2 + 1:)
              move path-to-excel           to path-launch(1:cont2)

              inspect path-launch replacing trailing spaces by low-value
                                
              string "START "    delimited by size
                     x"22222022" delimited by size
                     path-launch delimited by low-value
                     x"22"       delimited by size
                     into cmd-lancio
              end-string
              call "C$SYSTEM" using cmd-lancio, 193
                             giving Status-Run
              if status-run not = 0
                 perform DISPLAY-MSG-BOX
              end-if
           end-if.

      ***---
       CONTROLLA-CARTELLA.
           call "C$LIST-DIRECTORY" using LISTDIR-OPEN,
                                         wstampa,
                                         "*.*".
           move RETURN-CODE        to Dir-Handle.

           if Dir-Handle = ZERO
              set errori             to true
              display message box "Cartella export listini inesistente"
                      title titolo
                      icon 2
           else
              call "C$LIST-DIRECTORY" using LISTDIR-CLOSE, Dir-Handle
           end-if.

      ***---
       DATI-LUBEX.
           move zero   to spl-tipo-colonna
           set spl-stringa       to true.
           move arial5B  to spl-hfont.
           move 0,1              to spl-riga.
           move 3,4              to spl-colonna.
           move "LUBEX S.p.a."  to spl-riga-stampa
           call "spooler" using spooler-link.

           set spl-stringa       to true.
           move arial5  to spl-hfont.
           add 0,2              to spl-riga.
           move "Via G. Di Vittorio, 13/15"  to spl-riga-stampa
           call "spooler" using spooler-link.

           add 0,2              to spl-riga.
           move "20090 VIMODRONE"  to spl-riga-stampa
           call "spooler" using spooler-link.

           add 0,2              to spl-riga.
           move "(MILANO - ITALY)"  to spl-riga-stampa
           call "spooler" using spooler-link.

           add 0,4              to spl-riga.
           move ">> Tel:  02 26 51 551"  to spl-riga-stampa
           call "spooler" using spooler-link.

           add 0,2              to spl-riga.
           move ">> Fax: 02 26 515 549"  to spl-riga-stampa
           call "spooler" using spooler-link.

           add 4,1  to spl-colonna
           move 0,1              to spl-riga.

           add 0,2              to spl-riga.
           move "Capitale Sociale € 2.180.000 i.v."  to spl-riga-stampa
           call "spooler" using spooler-link.
           add 0,2              to spl-riga.
           move "P.IVA:   IT00785630963"  to spl-riga-stampa
           call "spooler" using spooler-link.


           add 3,1  to spl-colonna
           move 0,1              to spl-riga.

           add 0,2              to spl-riga.
           add 0,2              to spl-riga.

           initialize spl-riga-stampa
           string "Vimodrone, " delimited by size
                  como-data(7:2)   delimited by size
                  "/"              delimited by size
                  como-data(5:2)   delimited by size
                  "/"              delimited by size
                  como-data(1:4)   delimited by size
                  " "              delimited by size
                  como-ora(1:2)    delimited by size
                  "."              delimited by size
                  como-ora(3:2)    delimited by size
                  into spl-riga-stampa
           call "spooler" using spooler-link.

      ***---
       STAMPA-NOTE.
           set prima-nota to true
           move tlis-chiave  to  nlis-tlis-codice.
           move low-value    to  nlis-num-nota.
           move zero   to cont
           start nlistini key not < nlis-chiave
              invalid 
                 continue
              not invalid
                 perform until 1 = 2
                    read nlistini next 
                       at end 
                          exit perform 
                    end-read
                    if tlis-chiave not = nlis-tlis-codice
                       exit perform
                    end-if
                    add 1 to cont
                    if cont > 6
                       exit perform
                    end-if
                    perform SCRIVI-NOTA
                 end-perform
           end-start.

      ***---
       SCRIVI-NOTA.
           if prima-nota
              set prima-nota to false
              perform FINCATURA-NOTE
           end-if.

           add 0,30 to spl-riga.

           move zero    to spl-tipo-colonna
           move 1,3     to spl-colonna
           move courier to spl-hfont

           set spl-stringa   to true.
           move nlis-nota to spl-riga-stampa.
           call "spooler" using spooler-link. 

      ***---
       FINCATURA-NOTE.
           move zero   to spl-tipo-colonna
           move 8     to spl-pen-with.
           move 16,2  to spl-riga.
           add 2,4  to spl-riga giving spl-riga-fine
      
           move 0,6                to spl-colonna.
           add 27,8 to spl-colonna giving spl-colonna-fine.
      
           set  spl-oggetto        to true.
           set  SPL-RETTANGOLO-ROUND  to true.
           set  spl-brush-null     to true.
           set  spl-nero           to true.
           call "spooler"         using spooler-link.
      
           set  SPL-linea to true.
           move 4         to spl-pen-with.
           add 0,7        to spl-colonna
           subtract 0,1   from spl-colonna-fine
      
      *    righe interne
           add 0,3        to spl-riga
           perform 6 times
              add 0,3        to spl-riga
              move spl-riga  to spl-riga-fine
              call "spooler"         using spooler-link
           end-perform.

           set spl-stringa   to true.
           |move arial6       to spl-hfont.
           move arial6       to spl-hfont.
           subtract 1,80      from spl-riga.
           subtract 0,6      from spl-colonna.
           move "Note:"      to spl-riga-stampa
           call "spooler" using spooler-link.

           subtract 0,25      from spl-riga.


      ***---
       PARAGRAFO-COPY.
       copy "common-excel.cpy".
       copy "prz-finito-forn.cpy".
       copy "imposte-fornitore.cpy".
       copy "addizionale-piombo-fornitore.cpy".
       copy "costo-medio.cpy".
       copy "trova-parametro.cpy".

      ***---
       RECUPERO-ANAGRAFICA.

