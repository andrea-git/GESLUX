       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      val-saldi.
       AUTHOR.                          Andrea.
  
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "mtordini.sl".
           copy "clienti.sl".
           copy "destini.sl".


      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "mtordini.fd".
           copy "clienti.fd".
           copy "destini.fd".

       WORKING-STORAGE SECTION.

       77  status-mtordini pic xx.
       77  status-clienti  pic xx.
       77  status-destini  pic xx.

       77  tot             pic 9(6).

       PROCEDURE DIVISION.
      ***---
       MAIN.
           open i-o mtordini.
           open input clienti destini.

           move 0 to tot.
      *****     move low-value to mto-chiave.
      *****     start mtordini key >= mto-chiave.
      *****     perform until 1 = 2
      *****        read mtordini next at end exit perform end-read
      *****        if mto-prg-destino = 0
      *****           set cli-tipo-C to true
      *****           move mto-cod-cli to cli-codice
      *****           read clienti no lock 
      *****                invalid 
      *****                display message "MASTER " mto-chiave
      *****                         x"0D0A""CLIENTE NON VALIDO"
      *****                           icon 2
      *****           end-read
      *****           move cli-saldi-banco to mto-saldi-banco
      *****           move cli-saldi-promo to mto-saldi-promo
      *****        else
      *****           move mto-cod-cli     to des-codice
      *****           move mto-prg-destino to des-prog
      *****           read destini no lock
      *****                invalid 
      *****                display message "MASTER " mto-chiave
      *****                         x"0D0A""DESTINO NON VALIDO"
      *****                           icon 2
      *****           end-read
      *****           move des-saldi-banco to mto-saldi-banco
      *****           move des-saldi-promo to mto-saldi-promo
      *****        end-if
      *****        rewrite mto-rec
      *****        add 1 to tot
      *****     end-perform.
           display message "ELABORATI " tot " MASTER".
                 
           close mtordini clienti destini.

           goback.
