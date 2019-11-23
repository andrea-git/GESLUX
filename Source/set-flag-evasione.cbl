       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      set-flag-evasione.
       AUTHOR.                          Andrea.
       REMARKS. Associa a clienti destini i flag di evasione nel seguente modo:
                1. Esclusione Evadi tutto --> nessuno
                2. Evasione intera        --> nessuno
                3. Accorpamento master    --> tutto tranne evasione GDO
                4. Tenere saldi banco     --> in base all'attuale "tenere saldi"
                5. Tenere saldi promo     --> TUTTI
                6. gg validità volantino  --> 0
                7. Evasione 1             --> TUTTI
                8. Evasione 2             --> clienti di tipo "Evasione T" 
                9. Evasione 3             --> clienti di tipo "Evasione E" 
               10. Evasione 4             --> clienti di tipo "Evasione G"

                Impostare il flag saldi promo su master = master banco
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *****     copy "evaclides.sl". 
           copy "clienti.sl".
           copy "destini.sl".
           copy "ttipocli.sl".
           copy "mtordini.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
      *****     copy "evaclides.fd". 
           copy "clienti.fd".
           copy "destini.fd".
           copy "ttipocli.fd".
           copy "mtordini.fd".

       WORKING-STORAGE SECTION.
       78  titolo value "GESLUX- Settagio flag clienti per evasione".

      ***** 77  status-evaclides        pic x(2).
       77  status-clienti          pic x(2).
       77  status-destini          pic x(2).
       77  status-ttipocli         pic x(2).
       77  status-mtordini         pic x(2).

      ******************************************************************
       PROCEDURE DIVISION.

      ***---
       MAIN-PRG.
           perform INIT.
           perform OPEN-FILES.
           perform ELABORAZIONE.
           perform CLOSE-FILES.
           perform EXIT-PGM.

      ***---
       INIT.

      ***---
       OPEN-FILES.
      *****     open output evaclides.
      *****     open input  clienti.
      *****     open input  destini.
      *****     open input  ttipocli.
           open i-o    mtordini.
      
      ***---
       ELABORAZIONE.
      *****     move low-value to cli-rec.
      *****     set cli-tipo-C to true.
      *****     start clienti key >= cli-chiave.
      *****     perform until 1 = 2
      *****        read clienti next at end exit perform end-read
      *****        if cli-tipo-F exit perform end-if
      *****        initialize ecd-rec replacing numeric data by zeroes
      *****                                alphanumeric data by spaces
      *****        move cli-codice to ecd-cliente
      *****        move 0          to ecd-destino
      *****        |1. Esclusione Evadi tutto --> nessuno
      *****        set ecd-escludi-no to true
      *****
      *****        |2. Evasione intera        --> nessuno
      *****        set ecd-ev-intera-no to true
      *****
      *****        |3. Accorpamento master    --> tutto tranne evasione GDO
      *****        if tcl-evasione-GDO
      *****           set ecd-accorpa-no to true
      *****        else
      *****           set ecd-accorpa-si to true
      *****        end-if
      *****
      *****        |4. Tenere saldi banco     --> in base all'attuale "tenere saldi"
      *****        if cli-saldi-banco-si
      *****           set ecd-saldi-banco-si to true
      *****        else
      *****           set ecd-saldi-banco-no to true
      *****        end-if
      *****
      *****        |5. Tenere saldi promo     --> TUTTI
      *****        set ecd-saldi-promo-si to true
      *****
      *****        |6. gg validità volantino  --> 0
      *****        move 0 to ecd-gg-scadenza-vol
      *****
      *****        accept ecd-data-creazione from century-date
      *****        accept ecd-ora-creazione  from time
      *****        move "AUTO" to ecd-utente-creazione
      *****
      *****        | 7. Evasione 1  --> TUTTI
      *****        move 1 to ecd-el-evasione(1)
      *****
      *****        move cli-tipo to tcl-codice
      *****        read ttipocli no lock
      *****        | 8. Evasione 2  --> clienti di tipo "Evasione T" 
      *****        | 9. Evasione 3  --> clienti di tipo "Evasione E" 
      *****        |10. Evasione 4  --> clienti di tipo "Evasione G"
      *****        evaluate true
      *****        when tcl-evasione-TRAD   move 2 to ecd-el-evasione(2)
      *****        when tcl-evasione-ESTERO move 3 to ecd-el-evasione(3)
      *****        when tcl-evasione-GDO    move 4 to ecd-el-evasione(4)
      *****        end-evaluate
      *****
      *****        write ecd-rec
      *****        move low-value  to des-rec
      *****        move cli-codice to des-codice
      *****        start destini key >= des-chiave
      *****              invalid continue
      *****          not invalid
      *****              perform until 1 = 2
      *****                 read destini next at end exit perform end-read
      *****                 if des-codice not = cli-codice
      *****                    exit perform
      *****                 end-if
      *****                 move des-prog to ecd-destino
      *****
      *****                 |4. Tenere saldi banco     --> in base all'attuale "tenere saldi"
      *****                 if des-saldi-banco-si
      *****                    set ecd-saldi-banco-si to true
      *****                 else
      *****                    set ecd-saldi-banco-no to true
      *****                 end-if
      *****
      *****                 write ecd-rec
      *****              end-perform
      *****        end-start
      *****     end-perform.

           |Impostare il flag saldi promo su master = master banco
           move low-value to mto-rec.
           start mtordini key >= mto-chiave.
           perform until 1 = 2
              read mtordini next at end exit perform end-read
              set mto-saldi-promo-si to true
              rewrite mto-rec
           end-perform.

      ***---
       CLOSE-FILES.
           close mtordini. |ttipocli clienti destini evaclides.

      ***---
       EXIT-PGM.
           display message "FINE OPERAZIONE"
                     title titolo.
           goback.
