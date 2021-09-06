       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      convtordini.
       remarks. Per ampliamento numero ordine cliente
       ENVIRONMENT          DIVISION.
       CONFIGURATION        SECTION.
       SPECIAL-NAMES. decimal-point is comma.
      ******************************************************************

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "edi-mtordini.sl".

       SELECT EDI-mtordini-old
           ASSIGN       TO  "EDI-mtordini-old"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           LOCK MODE    IS MANUAL
           FILE STATUS  IS STATUS-EDI-mtordini-old
           RECORD KEY   IS old-emto-chiave
           ALTERNATE RECORD KEY IS old-emto-k-ord-cli = old-emto-anno, 
           old-emto-num-ord-cli
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS old-k-emto-stato = old-emto-stato, 
           old-emto-chiave
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS old-k-emto-data = 
           old-emto-data-ordine, 
           old-emto-chiave
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS old-k-emto-clides = old-emto-cod-cli, 
           old-emto-prg-destino, old-emto-chiave
           WITH DUPLICATES .


      *****************************************************************
       DATA DIVISION.
       FILE SECTION.          
           copy "edi-mtordini.fd".

       FD  EDI-mtordini-old.
       01 old-emto-rec.
           05 old-emto-chiave.
               10 old-emto-anno        PIC  9(4).
               10 old-emto-numero      PIC  9(8).
           05 old-emto-dati.
               10 old-emto-causale     PIC  x(4).
               10 old-emto-cod-cli     PIC  9(5).
               10 old-emto-prg-destino PIC  9(5).
               10 old-emto-gdo         PIC  x(5).
               10 old-emto-num-ord-cli PIC  X(50).
               10 old-emto-data-ordine PIC  9(8).
               10 old-emto-data-passaggio-ordine   PIC  9(8).
               10 old-emto-cod-agente  PIC  9(5).
               10 old-emto-cod-pagamento           PIC  x(3).
               10 old-emto-cod-ese-iva PIC  x(3).
               10 old-emto-vettore     PIC  9(5).
               10 old-emto-note1       PIC  X(19).
               10 old-emto-data-note1  PIC  9(8).
               10 old-emto-note2       PIC  X(30).
               10 old-emto-note3       PIC  X(30).
               10 old-emto-note4       PIC  X(30).
               10 old-emto-note        PIC  X(500).
               10 old-emto-pz-tot      PIC  9(8).
               10 old-emto-ritira-in-lubex         PIC  9.
               10 old-emto-prenotazione-qta        PIC  9(1).
               10 old-emto-saldi-banco PIC  9(1).
               10 old-emto-saldi-promo PIC  9(1).
               10 old-emto-stato       PIC  X(1). 
               10 old-emto-errori.
                   15 old-emto-cliente     PIC  9.
                   15 old-emto-cliente-fido            PIC  9.
                   15 old-emto-destino     PIC  9.
                   15 old-emto-righe       PIC  9.
                   15 old-emto-qta         PIC  9.
                   15 old-emto-art         PIC  9.
                   15 old-emto-prg         PIC  9.
                   15 old-emto-prz         PIC  9.
                   15 old-emto-esistente   PIC  9.
               10 old-emto-ordine.
                   15 old-emto-ordine-anno PIC  9(4).
                   15 old-emto-ordine-numero           PIC  9(8).
               10 old-emto-dati-comuni.
                   15 old-emto-data-creazione          PIC  9(8).
                   15 old-emto-ora-creazione           PIC  9(8).
                   15 old-emto-utente-creazione        PIC  X(10).
                   15 old-emto-data-ultima-modifica    PIC  9(8).
                   15 old-emto-ora-ultima-modifica     PIC  9(8).
                   15 old-emto-utente-ultima-modifica  PIC  X(10).
           05 old-emto-dati-import.
               15 old-emto-nome-file   PIC  x(100).
               15 old-emto-riga-file   PIC  9(6).
               15 old-emto-record-01T.
                   20 old-emto-01T-filler  PIC  x(35).
                   20 old-emto-02T-filler  PIC  x(35).
                   20 old-emto-03T-filler  PIC  x(35).
                   20 old-emto-01T04-BGM-DATADOC       PIC  x(8).
                   20 old-emto-01T05-BGM-NUMDOC        PIC  x(35).
                   20 old-emto-06T-filler  PIC  x(35).
                   20 old-emto-07T-filler  PIC  x(35).
                   20 old-emto-08T-filler  PIC  x(35).
                   20 old-emto-09T-filler  PIC  x(35).
                   20 old-emto-10T-filler  PIC  x(35).
                   20 old-emto-01T11-DTM-DATACONS      PIC  x(8).
                   20 old-emto-12T-filler  PIC  x(35).
                   20 old-emto-13T-filler  PIC  x(35).
                   20 old-emto-14T-filler  PIC  x(35).
                   20 old-emto-15T-filler  PIC  x(35).
                   20 old-emto-16T-filler  PIC  x(35).
                   20 old-emto-17T-filler  PIC  x(35).
                   20 old-emto-18T-filler  PIC  x(35).
                   20 old-emto-19T-filler  PIC  x(35).
                   20 old-emto-20T-filler  PIC  x(35).
                   20 old-emto-01T21-NAB-CODBUYER      PIC  x(17).
                   20 old-emto-01T22-NAB-QCODBUYER     PIC  x(35).
                   20 old-emto-01T23-NAB-RAGSOCB       PIC  x(70).
                   20 old-emto-01T24-NAB-INDIRB        PIC  x(35).
                   20 old-emto-01T25-NAB-CITTAB        PIC  x(35).
                   20 old-emto-01T26-NAB-PROVB         PIC  x(3).
                   20 old-emto-01T27-NAB-CAPB          PIC  x(5).
                   20 old-emto-01T28-NAD-CODCONS       PIC  x(17).
                   20 old-emto-29T-filler  PIC  x(35).
                   20 old-emto-01T30-NAD-RAGSOCD       PIC  x(70).
                   20 old-emto-01T31-NAD-INDIRD        PIC  x(35).
                   20 old-emto-01T32-NAD-CITTAD        PIC  x(35).
                   20 old-emto-01T33-NAD-PROVD         PIC  x(3).
                   20 old-emto-01T34-NAD-CAPD          PIC  x(5).
                   20 old-emto-01T35-FTX-NOTE          PIC  x(350).
                   20 old-emto-36T-filler  PIC  x(35).
                   20 old-emto-01T37-BGM-CODAZION      PIC  x(35).
                   20 old-emto-38T-filler  PIC  x(35).
                   20 old-emto-39T-filler  PIC  x(35).
                   20 old-emto-40T-filler  PIC  x(35).
                   20 old-emto-01T41-NAI-CODFATT       PIC  x(35).
                   20 old-emto-42T-filler  PIC  x(35).
                   20 old-emto-43T-filler  PIC  x(35).
                   20 old-emto-44T-filler  PIC  x(35).
                   20 old-emto-45T-filler  PIC  x(35).
                   20 old-emto-46T-filler  PIC  x(35).
                   20 old-emto-47T-filler  PIC  x(35).
                   20 old-emto-48T-filler  PIC  x(35).
                   20 old-emto-49T-filler  PIC  x(35).
                   20 old-emto-50T-filler  PIC  x(35).
                   20 old-emto-51T-filler  PIC  x(35).
                   20 old-emto-52T-filler  PIC  x(35).
                   20 old-emto-53T-filler  PIC  x(35).
                   20 old-emto-54T-filler  PIC  x(35).
                   20 old-emto-55T-filler  PIC  x(35).
                   20 old-emto-56T-filler  PIC  x(35).
                   20 old-emto-57T-filler  PIC  x(35).
                   20 old-emto-58T-filler  PIC  x(35).
                   20 old-emto-59T-filler  PIC  x(35).
                   20 old-emto-01T60-inversione-imposte  PIC  x.
                   20 old-emto-60T-filler  PIC  x(34).
                   20 old-emto-61T-filler  PIC  x(35).
                   20 old-emto-62T-filler  PIC  x(35).
                   20 old-emto-63T-filler  PIC  x(35).
                   20 old-emto-64T-filler  PIC  x(35).
                   20 old-emto-65T-filler  PIC  x(35).
                   20 old-emto-66T-filler  PIC  x(35).
                   20 old-emto-67T-filler  PIC  x(35).
                   20 old-emto-01T68-FTX-NOTE          PIC  x(350).
                   20 old-emto-01T69-FTX-NOTE          PIC  x(350).
                   20 old-emto-01T70-FTX-NOTE          PIC  x(350).
                   20 old-emto-71T-filler  PIC  x(35).
                   20 old-emto-72T-filler  PIC  x(35).
                   20 old-emto-73T-filler  PIC  x(35).
                   20 old-emto-74T-filler  PIC  x(35).
                   20 old-emto-75T-filler  PIC  x(35).
                   20 old-emto-76T-filler  PIC  x(35).
                   20 old-emto-77T-filler  PIC  x(35).
                   20 old-emto-78T-filler  PIC  x(35).
                   20 old-emto-79T-filler  PIC  x(35).
                   20 old-emto-80T-filler  PIC  x(35).
                   20 old-emto-81T-filler  PIC  x(35).
                   20 old-emto-82T-filler  PIC  x(35).
                   20 old-emto-83T-filler  PIC  x(35).
                   20 old-emto-84T-filler  PIC  x(35).
                   20 old-emto-85T-filler  PIC  x(35).
                   20 old-emto-86T-filler  PIC  x(35).
                   20 old-emto-87T-filler  PIC  x(35).
                   20 old-emto-88T-filler  PIC  x(35).
                   20 old-emto-89T-filler  PIC  x(35).
                   20 old-emto-90T-filler  PIC  x(35).
                   20 old-emto-91T-filler  PIC  x(35).
                   20 old-emto-92T-filler  PIC  x(35).
                   20 old-emto-93T-filler  PIC  x(35).
                   20 old-emto-94T-filler  PIC  x(35).
                   20 old-emto-95T-filler  PIC  x(35).
                   20 old-emto-96T-filler  PIC  x(35).
                   20 old-emto-97T-filler  PIC  x(35).
                   20 old-emto-98T-filler  PIC  x(35).
                   20 old-emto-99T-filler  PIC  x(35).
                   20 old-emto-100T-filler PIC  x(35).
                   20 old-emto-101T-filler PIC  x(35).
                   20 old-emto-102T-filler PIC  x(35).
                   20 old-emto-103T-filler PIC  x(35).
                   20 old-emto-104T-filler PIC  x(35).
                   20 old-emto-105T-filler PIC  x(35).
                   20 old-emto-106T-filler PIC  x(35).
                   20 old-emto-107T-filler PIC  x(35).
                   20 old-emto-108T-filler PIC  x(35).
                   20 old-emto-109T-filler PIC  x(35).
                   20 old-emto-110T-filler PIC  x(35).
                   20 old-emto-111T-filler PIC  x(35).
                   20 old-emto-112T-filler PIC  x(35).
                   20 old-emto-113T-filler PIC  x(35).
                   20 old-emto-114T-filler PIC  x(35).
                   20 old-emto-115T-filler PIC  x(35).
                   20 old-emto-116T-filler PIC  x(35).
                   20 old-emto-117T-filler PIC  x(35).
                   20 old-emto-118T-filler PIC  x(35).
                   20 old-emto-119T-filler PIC  x(35).
                   20 old-emto-120T-filler PIC  x(35).
                   20 old-emto-121T-filler PIC  x(35).
                   20 old-emto-122T-filler PIC  x(35).
                   20 old-emto-123T-filler PIC  x(35).
                   20 old-emto-124T-filler PIC  x(35).
                   20 old-emto-125T-filler PIC  x(35).
                   20 old-emto-126T-filler PIC  x(35).
                   20 old-emto-127T-filler PIC  x(35).
                   20 old-emto-128T-filler PIC  x(35).
                   20 old-emto-129T-filler PIC  x(35).
                   20 old-emto-130T-filler PIC  x(35).
                   20 old-emto-131T-filler PIC  x(35).
                   20 old-emto-132T-filler PIC  x(35).
                   20 old-emto-133T-filler PIC  x(35).
                   20 old-emto-134T-filler PIC  x(35).
                   20 old-emto-135T-filler PIC  x(35).
                   20 old-emto-136T-filler PIC  x(35).
                   20 old-emto-137T-filler PIC  x(35).
                   20 old-emto-138T-filler PIC  x(35).
                   20 old-emto-139T-filler PIC  x(35).
                   20 old-emto-140T-filler PIC  x(35).
                   20 old-emto-141T-filler PIC  x(35).
                   20 old-emto-142T-filler PIC  x(35).
                   20 old-emto-143T-filler PIC  x(35).
                   20 old-emto-144T-filler PIC  x(35).
                   20 old-emto-145T-filler PIC  x(35).
                   20 old-emto-146T-filler PIC  x(35).
                   20 old-emto-147T-filler PIC  x(35).
                   20 old-emto-148T-filler PIC  x(35).
                   20 old-emto-149T-filler PIC  x(35).
                   20 old-emto-150T-filler PIC  x(35).
                   20 old-emto-151T-filler PIC  x(35).
                   20 old-emto-152T-filler PIC  x(35).
                   20 old-emto-153T-filler PIC  x(35).
                   20 old-emto-154T-filler PIC  x(35).
                   20 old-emto-155T-filler PIC  x(35).
                   20 old-emto-156T-filler PIC  x(35).
                   20 old-emto-157T-filler PIC  x(35).
                   20 old-emto-158T-filler PIC  x(35).
                   20 old-emto-159T-filler PIC  x(35).
                   20 old-emto-160T-filler PIC  x(35).
                   20 old-emto-161T-filler PIC  x(35).
                   20 old-emto-162T-filler PIC  x(35).
                   20 old-emto-01T163-TOD-CODCOST      PIC  x(3).
                   20 filler           PIC  x(501).
               15 old-emto-bloc-forzato            PIC  9.
               15 old-emto-Sum         PIC  9(12)v999.
               15 old-emto-evadi-dal   PIC  9(8).
               15 old-emto-inversione-imposte      PIC  9.
               15 FILLER           PIC  x(175).


       WORKING-STORAGE SECTION.
           copy "acucobol.def".
           copy "acugui.def".
       77  status-edi-mtordini       pic X(2).
       77  status-edi-mtordini-old   pic X(2).

       77  CONT                 PIC 9(3).
       77  CONT-ED              PIC Z(3).
       77  scelta               pic 9.

      ******************************************************************
       PROCEDURE DIVISION.     
           COPY "DECLXER1".
                     edi-mtordini
                     edi-mtordini-old
                     .
           COPY "DECLXER2".

       MAIN-PRG.           
           display message box 
                        "Confermi la conversione del file edi-mtordini?"
                          x"0D0A"
                          "Prima di procedere con la conversione"
                          x"0D0A"
                          "rinominare il file "
                          x"22"
                          "edi-mtordini"
                          x"22"
                          " in "
                          x"22"
                          "edi-mtordini-old"
                          x"22"
                          "."
                          type mb-yes-no
                          default mb-no
                          giving scelta
                          icon 2
           if scelta = mb-yes
              perform CONVERSIONE
           end-if.

           goback.


      ***---
       CONVERSIONE.
           move zero   to cont

           open input  edi-mtordini-old.
           open output edi-mtordini


           move low-value to old-emto-chiave.

           start edi-mtordini-old key not less old-emto-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read edi-mtordini-old next
                       at end
                          exit perform
                    end-read

                    perform MUOVI-RECORD

                 end-perform
           end-start.

           close edi-mtordini
                 edi-mtordini-old.

           move cont   to cont-ed
           display message box "Convertiti " cont-ed " record.".


      ***---
       MUOVI-RECORD.
           add 1 to cont.
           initialize old-emto-rec replacing numeric data by zeroes
                                        alphanumeric data by spaces.      

           write emto-rec.     
