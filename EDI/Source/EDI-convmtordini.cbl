       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      EDI-convmtordini.

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
      *****
      *****     copy "edi-mrordini.sl".
      *****
      ***** SELECT EDI-mrordini-old
      *****     ASSIGN       TO  "EDI-mrordini-old"
      *****     ORGANIZATION IS INDEXED
      *****     ACCESS MODE  IS DYNAMIC
      *****     LOCK MODE    IS MANUAL
      *****     FILE STATUS  IS STATUS-EDI-mrordini-old
      *****     RECORD KEY   IS old-emro-chiave
      *****     ALTERNATE RECORD KEY IS old-emro-k-articolo = 
      *****     old-emro-cod-articolo, old-emro-chiave
      *****     WITH DUPLICATES 
      *****     ALTERNATE RECORD KEY IS old-emro-k-stato = old-emro-stato, 
      *****     old-emro-chiave
      *****     WITH DUPLICATES .

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.          
           copy "edi-mtordini.fd".
           
       FD  EDI-mtordini-old.
       01 OLD-emto-rec.
           05 OLD-emto-chiave.
               10 OLD-emto-anno        PIC  9(4).
               10 OLD-emto-numero      PIC  9(8).
           05 OLD-emto-dati.
               10 OLD-emto-causale     PIC  x(4).
               10 OLD-emto-cod-cli     PIC  9(5).
               10 OLD-emto-prg-destino PIC  9(5).
               10 OLD-emto-gdo         PIC  x(5).
               10 OLD-emto-num-ord-cli PIC  X(10).
               10 OLD-emto-data-ordine PIC  9(8).
               10 OLD-emto-data-passaggio-ordine   PIC  9(8).
               10 OLD-emto-cod-agente  PIC  9(5).
               10 OLD-emto-cod-pagamento           PIC  x(3).
               10 OLD-emto-cod-ese-iva PIC  x(3).
               10 OLD-emto-vettore     PIC  9(5).
               10 OLD-emto-note1       PIC  X(19).
               10 OLD-emto-data-note1  PIC  9(8).
               10 OLD-emto-note2       PIC  X(30).
               10 OLD-emto-note3       PIC  X(30).
               10 OLD-emto-note4       PIC  X(30).
               10 OLD-emto-note        PIC  X(500).
               10 OLD-emto-pz-tot      PIC  9(8).
               10 OLD-emto-ritira-in-lubex         PIC  9.
                   88 OLD-emto-ritira-si VALUE IS 1. 
                   88 OLD-emto-ritira-no VALUE IS 0. 
               10 OLD-emto-prenotazione-qta        PIC  9(1).
                   88 OLD-emto-prenotazione-qta-si VALUE IS 1. 
                   88 OLD-emto-prenotazione-qta-no VALUE IS 0. 
               10 OLD-emto-saldi-banco PIC  9(1).
                   88 OLD-emto-saldi-banco-si VALUE IS 1. 
                   88 OLD-emto-saldi-banco-no VALUE IS 0. 
               10 OLD-emto-saldi-promo PIC  9(1).
                   88 OLD-emto-saldi-promo-si VALUE IS 1. 
                   88 OLD-emto-saldi-promo-no VALUE IS 0. 
               10 OLD-emto-stato       PIC  X(1).
                   88 OLD-emto-attivo VALUE IS "A". 
                   88 OLD-emto-bloccato VALUE IS "B". 
                   88 OLD-emto-caricato VALUE IS "C". 
               10 OLD-emto-errori.
                   15 OLD-emto-cliente     PIC  9.
                       88 OLD-emto-cliente-valido VALUE IS 0. 
                       88 OLD-emto-clides-non-valido VALUE IS 1. 
                       88 OLD-emto-cliente-non-valido VALUE IS 2. 
                       88 OLD-emto-cliente-non-attivo VALUE IS 3. 
                   15 OLD-emto-cliente-fido            PIC  9.
                       88 OLD-emto-cliente-fido-ok VALUE IS 0. 
                       88 OLD-emto-cliente-fuori-fido VALUE IS 1. 
                   15 OLD-emto-destino     PIC  9.
                       88 OLD-emto-destino-valido VALUE IS 0. 
                       88 OLD-emto-destino-non-valido VALUE IS 1. 
                       88 OLD-emto-destino-non-attivo VALUE IS 2. 
                   15 OLD-emto-righe       PIC  9.
                       88 OLD-emto-righe-presenti VALUE IS 0. 
                       88 OLD-emto-righe-non-presenti VALUE IS 1. 
                   15 OLD-emto-qta         PIC  9.
                       88 OLD-emto-qta-ok VALUE IS 0. 
                       88 OLD-emto-qta-ko VALUE IS 1. 
                   15 OLD-emto-art         PIC  9.
                       88 OLD-emto-art-ok VALUE IS 0. 
                       88 OLD-emto-art-ko VALUE IS 1. 
                   15 OLD-emto-prg         PIC  9.
                       88 OLD-emto-prg-ok VALUE IS 0. 
                       88 OLD-emto-prg-ko VALUE IS 1. 
                   15 OLD-emto-prz         PIC  9.
                       88 OLD-emto-prz-ok VALUE IS 0. 
                       88 OLD-emto-prz-ko VALUE IS 1. 
                   15 OLD-emto-esistente   PIC  9.
                       88 OLD-emto-esistente-no VALUE IS 0. 
                       88 OLD-emto-esistente-si VALUE IS 1. 
               10 OLD-emto-ordine.
                   15 OLD-emto-ordine-anno PIC  9(4).
                   15 OLD-emto-ordine-numero           PIC  9(8).
               10 OLD-emto-dati-comuni.
                   15 OLD-emto-data-creazione          PIC  9(8).
                   15 OLD-emto-ora-creazione           PIC  9(8).
                   15 OLD-emto-utente-creazione        PIC  X(10).
                   15 OLD-emto-data-ultima-modifica    PIC  9(8).
                   15 OLD-emto-ora-ultima-modifica     PIC  9(8).
                   15 OLD-emto-utente-ultima-modifica  PIC  X(10).
           05 OLD-emto-dati-import.
               15 OLD-emto-nome-file   PIC  x(100).
               15 OLD-emto-riga-file   PIC  9(6).
               15 OLD-emto-record-01T.
                   20 OLD-emto-01T-filler  PIC  x(35).
                   20 OLD-emto-02T-filler  PIC  x(35).
                   20 OLD-emto-03T-filler  PIC  x(35).
                   20 OLD-emto-01T04-BGM-DATADOC       PIC  x(8).
                   20 OLD-emto-01T05-BGM-NUMDOC        PIC  x(35).
                   20 OLD-emto-06T-filler  PIC  x(35).
                   20 OLD-emto-07T-filler  PIC  x(35).
                   20 OLD-emto-08T-filler  PIC  x(35).
                   20 OLD-emto-09T-filler  PIC  x(35).
                   20 OLD-emto-10T-filler  PIC  x(35).
                   20 OLD-emto-01T11-DTM-DATACONS      PIC  x(8).
                   20 OLD-emto-12T-filler  PIC  x(35).
                   20 OLD-emto-13T-filler  PIC  x(35).
                   20 OLD-emto-14T-filler  PIC  x(35).
                   20 OLD-emto-15T-filler  PIC  x(35).
                   20 OLD-emto-16T-filler  PIC  x(35).
                   20 OLD-emto-17T-filler  PIC  x(35).
                   20 OLD-emto-18T-filler  PIC  x(35).
                   20 OLD-emto-19T-filler  PIC  x(35).
                   20 OLD-emto-20T-filler  PIC  x(35).
                   20 OLD-emto-01T21-NAB-CODBUYER      PIC  x(17).
                   20 OLD-emto-22T-filler  PIC  x(35).
                   20 OLD-emto-01T23-NAB-RAGSOCB       PIC  x(70).
                   20 OLD-emto-01T24-NAB-INDIRB        PIC  x(35).
                   20 OLD-emto-01T25-NAB-CITTAB        PIC  x(35).
                   20 OLD-emto-01T26-NAB-PROVB         PIC  x(3).
                   20 OLD-emto-01T27-NAB-CAPB          PIC  x(5).
                   20 OLD-emto-01T28-NAD-CODCONS       PIC  x(17).
                   20 OLD-emto-29T-filler  PIC  x(35).
                   20 OLD-emto-01T30-NAD-RAGSOCD       PIC  x(70).
                   20 OLD-emto-01T31-NAD-INDIRD        PIC  x(35).
                   20 OLD-emto-01T32-NAD-CITTAD        PIC  x(35).
                   20 OLD-emto-01T33-NAD-PROVD         PIC  x(3). 
                   20 OLD-emto-01T34-NAD-CAPD          PIC  x(5).
                   20 OLD-emto-01T35-FTX-NOTE          PIC  x(350).
                   20 OLD-emto-36T-filler  PIC  x(35).
                   20 OLD-emto-37T-filler  PIC  x(35).
                   20 OLD-emto-38T-filler  PIC  x(35).
                   20 OLD-emto-39T-filler  PIC  x(35).
                   20 OLD-emto-40T-filler  PIC  x(35).
                   20 OLD-emto-41T-filler  PIC  x(35).
                   20 OLD-emto-42T-filler  PIC  x(35).
                   20 OLD-emto-43T-filler  PIC  x(35).
                   20 OLD-emto-44T-filler  PIC  x(35).
                   20 OLD-emto-45T-filler  PIC  x(35).
                   20 OLD-emto-46T-filler  PIC  x(35).
                   20 OLD-emto-47T-filler  PIC  x(35).
                   20 OLD-emto-48T-filler  PIC  x(35).
                   20 OLD-emto-49T-filler  PIC  x(35).
                   20 OLD-emto-50T-filler  PIC  x(35).
                   20 OLD-emto-51T-filler  PIC  x(35).
                   20 OLD-emto-52T-filler  PIC  x(35).
                   20 OLD-emto-53T-filler  PIC  x(35).
                   20 OLD-emto-54T-filler  PIC  x(35).
                   20 OLD-emto-55T-filler  PIC  x(35).
                   20 OLD-emto-56T-filler  PIC  x(35).
                   20 OLD-emto-57T-filler  PIC  x(35).
                   20 OLD-emto-58T-filler  PIC  x(35).
                   20 OLD-emto-59T-filler  PIC  x(35).
                   20 OLD-emto-60T-filler  PIC  x(35).
                   20 OLD-emto-61T-filler  PIC  x(35).
                   20 OLD-emto-62T-filler  PIC  x(35).
                   20 OLD-emto-63T-filler  PIC  x(35).
                   20 OLD-emto-64T-filler  PIC  x(35).
                   20 OLD-emto-65T-filler  PIC  x(35).
                   20 OLD-emto-66T-filler  PIC  x(35).
                   20 OLD-emto-67T-filler  PIC  x(35).
                   20 OLD-emto-01T68-FTX-NOTE          PIC  x(350).
                   20 OLD-emto-01T69-FTX-NOTE          PIC  x(350).
                   20 OLD-emto-01T70-FTX-NOTE          PIC  x(350).
                   20 OLD-emto-71T-filler  PIC  x(35).
                   20 OLD-emto-72T-filler  PIC  x(35).
                   20 OLD-emto-73T-filler  PIC  x(35).
                   20 OLD-emto-74T-filler  PIC  x(35).
                   20 OLD-emto-75T-filler  PIC  x(35).
                   20 OLD-emto-76T-filler  PIC  x(35).
                   20 OLD-emto-77T-filler  PIC  x(35).
                   20 OLD-emto-78T-filler  PIC  x(35).
                   20 OLD-emto-79T-filler  PIC  x(35).
                   20 OLD-emto-80T-filler  PIC  x(35).
                   20 OLD-emto-81T-filler  PIC  x(35).
                   20 OLD-emto-82T-filler  PIC  x(35).
                   20 OLD-emto-83T-filler  PIC  x(35).
                   20 OLD-emto-84T-filler  PIC  x(35).
                   20 OLD-emto-85T-filler  PIC  x(35).
                   20 OLD-emto-86T-filler  PIC  x(35).
                   20 OLD-emto-87T-filler  PIC  x(35).
                   20 OLD-emto-88T-filler  PIC  x(35).
                   20 OLD-emto-89T-filler  PIC  x(35).
                   20 OLD-emto-90T-filler  PIC  x(35).
                   20 OLD-emto-91T-filler  PIC  x(35).
                   20 OLD-emto-92T-filler  PIC  x(35).
                   20 OLD-emto-93T-filler  PIC  x(35).
                   20 OLD-emto-94T-filler  PIC  x(35).
                   20 OLD-emto-95T-filler  PIC  x(35).
                   20 OLD-emto-96T-filler  PIC  x(35).
                   20 OLD-emto-97T-filler  PIC  x(35).
                   20 OLD-emto-98T-filler  PIC  x(35).
                   20 OLD-emto-99T-filler  PIC  x(35).
                   20 OLD-emto-100T-filler PIC  x(35).
                   20 OLD-emto-101T-filler PIC  x(35).
                   20 OLD-emto-102T-filler PIC  x(35).
                   20 OLD-emto-103T-filler PIC  x(35).
                   20 OLD-emto-104T-filler PIC  x(35).
                   20 OLD-emto-105T-filler PIC  x(35).
                   20 OLD-emto-106T-filler PIC  x(35).
                   20 OLD-emto-107T-filler PIC  x(35).
                   20 OLD-emto-108T-filler PIC  x(35).
                   20 OLD-emto-109T-filler PIC  x(35).
                   20 OLD-emto-110T-filler PIC  x(35).
                   20 OLD-emto-111T-filler PIC  x(35).
                   20 OLD-emto-112T-filler PIC  x(35).
                   20 OLD-emto-113T-filler PIC  x(35).
                   20 OLD-emto-114T-filler PIC  x(35).
                   20 OLD-emto-115T-filler PIC  x(35).
                   20 OLD-emto-116T-filler PIC  x(35).
                   20 OLD-emto-117T-filler PIC  x(35).
                   20 OLD-emto-118T-filler PIC  x(35).
                   20 OLD-emto-119T-filler PIC  x(35).
                   20 OLD-emto-120T-filler PIC  x(35).
                   20 OLD-emto-121T-filler PIC  x(35).
                   20 OLD-emto-122T-filler PIC  x(35).
                   20 OLD-emto-123T-filler PIC  x(35).
                   20 OLD-emto-124T-filler PIC  x(35).
                   20 OLD-emto-125T-filler PIC  x(35).
                   20 OLD-emto-126T-filler PIC  x(35).
                   20 OLD-emto-127T-filler PIC  x(35).
                   20 OLD-emto-128T-filler PIC  x(35).
                   20 OLD-emto-129T-filler PIC  x(35).
                   20 OLD-emto-130T-filler PIC  x(35).
                   20 OLD-emto-131T-filler PIC  x(35).
                   20 OLD-emto-132T-filler PIC  x(35).
                   20 OLD-emto-133T-filler PIC  x(35).
                   20 OLD-emto-134T-filler PIC  x(35).
                   20 OLD-emto-135T-filler PIC  x(35).
                   20 OLD-emto-136T-filler PIC  x(35).
                   20 OLD-emto-137T-filler PIC  x(35).
                   20 OLD-emto-138T-filler PIC  x(35).
                   20 OLD-emto-139T-filler PIC  x(35).
                   20 OLD-emto-140T-filler PIC  x(35).
                   20 OLD-emto-141T-filler PIC  x(35).
                   20 OLD-emto-142T-filler PIC  x(35).
                   20 OLD-emto-143T-filler PIC  x(35).
                   20 OLD-emto-144T-filler PIC  x(35).
                   20 OLD-emto-145T-filler PIC  x(35).
                   20 OLD-emto-146T-filler PIC  x(35).
                   20 OLD-emto-147T-filler PIC  x(35).
                   20 OLD-emto-148T-filler PIC  x(35).
                   20 OLD-emto-149T-filler PIC  x(35).
                   20 OLD-emto-150T-filler PIC  x(35).
                   20 OLD-emto-151T-filler PIC  x(35).
                   20 OLD-emto-152T-filler PIC  x(35).
                   20 OLD-emto-153T-filler PIC  x(35).
                   20 OLD-emto-154T-filler PIC  x(35).
                   20 OLD-emto-155T-filler PIC  x(35).
                   20 OLD-emto-156T-filler PIC  x(35).
                   20 OLD-emto-157T-filler PIC  x(35).
                   20 OLD-emto-158T-filler PIC  x(35).
                   20 OLD-emto-159T-filler PIC  x(35).
                   20 OLD-emto-160T-filler PIC  x(35).
                   20 OLD-emto-161T-filler PIC  x(35).
                   20 OLD-emto-162T-filler PIC  x(35).
                   20 OLD-emto-01T163-TOD-CODCOST      PIC  x(3).
               15 FILLER           PIC  x(200).

      ***** copy "EDI-mrordini.fd".
      *****
      ***** FD  EDI-mrordini-old.
      ***** 01 old-emro-rec.
      *****     05 old-emro-chiave.
      *****         10 old-emro-chiave-testa.
      *****             15 old-emro-anno        PIC  9(4).
      *****             15 old-emro-numero      PIC  9(8).
      *****         10 old-emro-riga        PIC  9(5).
      *****     05 old-emro-dati.
      *****         10 old-emro-cod-articolo            PIC  9(6).
      *****         10 old-emro-qta-EDI     PIC  9(8).
      *****         10 old-emro-qta-GESLUX  PIC  9(8).
      *****         10 old-emro-qta         PIC  9(8).
      *****         10 old-emro-prz-EDI     PIC  9(9)v9(2).
      *****         10 old-emro-prz-GESLUX  PIC  9(9)v9(2).
      *****         10 old-emro-prz         PIC  9(9)v9(2).
      *****         10 old-emro-bloccato-prezzo         PIC  9.
      *****             88 old-emro-bloccato-prezzo-no VALUE IS 0. 
      *****             88 old-emro-bloccato-prezzo-si VALUE IS 1. 
      *****         10 old-emro-peso-utf    PIC  9(5)v9(3).
      *****         10 old-emro-peso-non-utf            PIC  9(5)v9(3).
      *****         10 old-emro-num-colli   PIC  9(5).
      *****         10 old-emro-cod-imballo PIC  X(3).
      *****         10 old-emro-des-imballo PIC  X(50).
      *****         10 old-emro-qta-imballi PIC  9(4).
      *****         10 old-emro-cod-art-cli PIC  X(15).
      *****         10 old-emro-prz-commle  PIC  9(9)v9(2).
      *****         10 old-emro-prg-chiave.
      *****             15 old-emro-prg-cod-articolo        PIC  9(6).
      *****             15 old-emro-prg-cod-magazzino       PIC  X(3).
      *****             15 old-emro-prg-tipo-imballo        PIC  X(3).
      *****             15 old-emro-prg-peso    PIC  9(5)v9(3).
      *****         10 old-emro-prg-forzato.
      *****             15 old-emro-prg-cod-articolo-f      PIC  9(6).
      *****             15 old-emro-prg-cod-magazzino-f     PIC  X(3).
      *****             15 old-emro-prg-tipo-imballo-f      PIC  X(3).
      *****             15 old-emro-prg-peso-f  PIC  9(5)v9(3).
      *****         10 old-emro-dati-blister.
      *****             15 old-emro-bli-codice  PIC  9(6).
      *****             15 old-emro-bli-qta     PIC  9(8).
      *****             15 old-emro-bli-perce   PIC  9(3)v99.
      *****             15 old-emro-blister     PIC  9.
      *****         10 old-emro-promo       PIC  9(15).
      *****         10 old-emro-prz-promo   PIC  9.
      *****             88 old-emro-si-prz-promo VALUE IS 1. 
      *****             88 old-emro-no-prz-promo VALUE IS 0. 
      *****         10 old-emro-evadi-dal   PIC  9(8).
      *****         10 old-emro-stato       PIC  X(1).     
      *****         10 old-emro-errori.
      *****             15 old-emro-articolo    PIC  9.
      *****             15 old-emro-qtac        PIC  9.
      *****             15 old-emro-prezzo      PIC  9.
      *****             15 old-emro-progressivo PIC  9.
      *****         10 old-emro-ordine.
      *****             15 old-emro-ordine-testa.
      *****                 20 old-emro-ordine-anno PIC  9(4).
      *****                 20 old-emro-ordine-numero           PIC  9(8).
      *****             15 old-emro-ordine-riga PIC  9(5).
      *****         10 old-emro-dati-comuni.
      *****             15 old-emro-data-creazione          PIC  9(8).
      *****             15 old-emro-ora-creazione           PIC  9(8).
      *****             15 old-emro-utente-creazione        PIC  X(10).
      *****             15 old-emro-data-ultima-modifica    PIC  9(8).
      *****             15 old-emro-ora-ultima-modifica     PIC  9(8).
      *****             15 old-emro-utente-ultima-modifica  PIC  X(10).
      *****         10 old-emro-dati-import.
      *****             15 old-emro-nome-file   PIC  x(100).
      *****             15 old-emro-riga-file   PIC  9(6).
      *****             15 old-emro-record-02D.
      *****                 20 old-emro-01D-filler  PIC  x(1).
      *****                 20 old-emro-02D-filler  PIC  x(1).
      *****                 20 old-emro-03D-filler  PIC  x(1).
      *****                 20 old-emro-04D-filler  PIC  x(1).
      *****                 20 old-emro-05D-filler  PIC  x(1).
      *****                 20 old-emro-06D-filler  PIC  x(1).
      *****                 20 old-emro-07D-filler  PIC  x(1).
      *****                 20 old-emro-08D-filler  PIC  x(1).
      *****                 20 old-emro-09D-filler  PIC  x(1).
      *****                 20 old-emro-10D-filler  PIC  x(1).
      *****                 20 old-emro-11D-filler  PIC  x(1).
      *****                 20 old-emro-12D-filler  PIC  x(1).
      *****                 20 old-emro-02D13-LIN-CODFORTU      PIC  x(35).
      *****                 20 old-emro-14D-filler      PIC  x(1).
      *****                 20 old-emro-02D15-LIN-DESART        PIC  x(100).
      *****                 20 old-emro-16D-filler  PIC  x(1).
      *****                 20 old-emro-02D17-QTAORD            PIC  x(20).
      *****                 20 old-emro-18D-filler  PIC  x(1).
      *****                 20 old-emro-02D19-LIN-PRZUNI        PIC  x(20).
      *****                 20 old-emro-19D-filler  PIC  x(1).
      *****                 20 old-emro-20D-filler  PIC  x(1).
      *****                 20 old-emro-21D-filler  PIC  x(1).
      *****                 20 old-emro-02D22-LIN-NRCUINTU      PIC  x(20).
      *****                 20 old-emro-02D222-QTAPZ            PIC  x(20).
      *****             15 FILLER           PIC  x(180).

       WORKING-STORAGE SECTION.
           copy "acucobol.def".
           copy "acugui.def".
       77  status-edi-mtordini      pic X(2).
       77  status-edi-mtordini-old  pic X(2).
      ***** 77  status-edi-mrordini      pic X(2).
      ***** 77  status-edi-mrordini-old  pic X(2).

       77  CONT                 PIC 9(5).
       77  CONT-ED              PIC Z(5).
       77  scelta               pic 9.
       77  idx                  pic 9(5).

      ******************************************************************
       PROCEDURE DIVISION.     
           COPY "DECLXER1".
                     edi-mtordini
                     edi-mtordini-old
      *****               edi-mrordini
      *****               edi-mrordini-old
                     .
           COPY "DECLXER2".

       MAIN-PRG.           
           display message box 
                        "Confermi la conversione del file EDI-mtordini?"
                          x"0D0A"
                          "Prima di procedere con la conversione"
                          x"0D0A"
                          "rinominare il file "
                          x"22"
                          "EDI-mtordini"
                          x"22"
                          " in "
                          x"22"
                          "EDI-mtordini-old"
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
           open output edi-mtordini.

           move low-value to old-emto-chiave.

           start edi-mtordini-old key >= old-emto-chiave
                 invalid continue
             not invalid                        
                 perform until 1 = 2
                    read edi-mtordini-old next 
                         at end exit perform 
                    end-read

                    perform MUOVI-RECORD-T

                 end-perform
           end-start.

           close edi-mtordini
                 edi-mtordini-old

           move cont   to cont-ed
           display message box "Convertiti " cont-ed " record.".

      *****     move zero   to cont
      *****
      *****     open input  edi-mrordini-old.
      *****     open output edi-mrordini.
      *****
      *****     move low-value to old-emro-chiave.
      *****
      *****     start edi-mrordini-old key >= old-emro-chiave
      *****           invalid continue
      *****       not invalid                        
      *****           perform until 1 = 2
      *****              read edi-mrordini-old next 
      *****                   at end exit perform 
      *****              end-read
      *****
      *****              perform MUOVI-RECORD-R
      *****
      *****           end-perform
      *****     end-start.
      *****
      *****     close edi-mrordini
      *****           edi-mrordini-old
      *****
      *****     move cont   to cont-ed
      *****     display message box "Convertiti " cont-ed " record.".
      *****
                         
      ***---
       MUOVI-RECORD-T.
           add 1 to cont.
           initialize emto-rec replacing numeric data by zeroes
                                    alphanumeric data by spaces.
           move old-emto-chiave                to emto-chiave.
           move old-emto-causale               to emto-causale.
           move old-emto-cod-cli               to emto-cod-cli.
           move old-emto-prg-destino           to emto-prg-destino.          
           move old-emto-gdo                   to emto-gdo.
           move old-emto-num-ord-cli           to emto-num-ord-cli.
           move old-emto-data-ordine           to emto-data-ordine.
           move old-emto-data-passaggio-ordine 
             to emto-data-passaggio-ordine.
           move old-emto-cod-agente            to emto-cod-agente.
           move old-emto-cod-pagamento         to emto-cod-pagamento.
           move old-emto-cod-ese-iva           to emto-cod-ese-iva.
           move old-emto-vettore               to emto-vettore.
           move old-emto-note1                 to emto-note1.
           move old-emto-data-note1            to emto-data-note1.
           move old-emto-note2                 to emto-note2.
           move old-emto-note3                 to emto-note3.
           move old-emto-note4                 to emto-note4.
           move old-emto-note                  to emto-note.
           move old-emto-pz-tot                to emto-pz-tot.
           move old-emto-ritira-in-lubex       to emto-ritira-in-lubex.
           move old-emto-prenotazione-qta      to emto-prenotazione-qta.
           move old-emto-saldi-banco           to emto-saldi-banco.
           move old-emto-saldi-promo           to emto-saldi-promo.
           move old-emto-stato                 to emto-stato.
           move old-emto-errori                to emto-errori.
           move old-emto-ordine                to emto-ordine.
           move old-emto-dati-comuni           to emto-dati-comuni.
           move old-emto-01T04-BGM-DATADOC    to emto-01T04-BGM-DATADOC  
           move old-emto-01T05-BGM-NUMDOC     to emto-01T05-BGM-NUMDOC   
           move old-emto-01T11-DTM-DATACONS   to emto-01T11-DTM-DATACONS 
           move old-emto-01T21-NAB-CODBUYER   to emto-01T21-NAB-CODBUYER 
           move old-emto-01T28-NAD-CODCONS    to emto-01T28-NAD-CODCONS  
           move old-emto-01T35-FTX-NOTE       to emto-01T35-FTX-NOTE     
           move old-emto-01T68-FTX-NOTE       to emto-01T68-FTX-NOTE     
           move old-emto-01T69-FTX-NOTE       to emto-01T69-FTX-NOTE     
           move old-emto-01T70-FTX-NOTE       to emto-01T70-FTX-NOTE     
           move old-emto-01T163-TOD-CODCOST   to emto-01T163-TOD-CODCOST 
                   
           write emto-rec.     


      ********---
      ***** MUOVI-RECORD-R.
      *****     add 1 to cont.
      *****     initialize emro-rec replacing numeric data by zeroes
      *****                              alphanumeric data by spaces.
      *****     move old-emro-chiave          to emro-chiave          
      *****     move old-emro-cod-articolo    to emro-cod-articolo    
      *****     move old-emro-qta-EDI         to emro-qta-EDI         
      *****     move old-emro-qta-GESLUX      to emro-qta-GESLUX      
      *****     move old-emro-qta             to emro-qta             
      *****     move old-emro-prz-EDI         to emro-prz-EDI         
      *****     move old-emro-prz-GESLUX      to emro-prz-GESLUX      
      *****     move old-emro-prz             to emro-prz             
      *****     move old-emro-bloccato-prezzo to emro-bloccato-prezzo 
      *****     move old-emro-peso-utf        to emro-peso-utf        
      *****     move old-emro-peso-non-utf    to emro-peso-non-utf    
      *****     move old-emro-num-colli       to emro-num-colli       
      *****     move old-emro-cod-imballo     to emro-cod-imballo     
      *****     move old-emro-des-imballo     to emro-des-imballo     
      *****     move old-emro-qta-imballi     to emro-qta-imballi     
      *****     move old-emro-cod-art-cli     to emro-cod-art-cli     
      *****     move old-emro-prz-commle      to emro-prz-commle      
      *****     move old-emro-prg-chiave      to emro-prg-chiave      
      *****     move old-emro-prg-forzato     to emro-prg-forzato     
      *****     move old-emro-dati-blister    to emro-dati-blister    
      *****     move old-emro-promo           to emro-promo           
      *****     move old-emro-prz-promo       to emro-prz-promo       
      *****     move old-emro-evadi-dal       to emro-evadi-dal       
      *****     move old-emro-stato           to emro-stato           
      *****     move old-emro-errori          to emro-errori          
      *****     move old-emro-ordine          to emro-ordine          
      *****     move old-emro-dati-comuni     to emro-dati-comuni     
      *****                              
      *****     move old-emro-nome-file      to emro-nome-file
      *****     move old-emro-riga-file      to emro-riga-file
      *****     move old-emro-01D-filler      to emro-01D-filler
      *****     move old-emro-02D-filler      to emro-02D-filler
      *****     move old-emro-03D-filler      to emro-03D-filler
      *****     move old-emro-04D-filler      to emro-04D-filler
      *****     move old-emro-05D-filler      to emro-05D-filler
      *****     move old-emro-06D-filler      to emro-06D-filler
      *****     move old-emro-07D-filler      to emro-07D-filler
      *****     move old-emro-08D-filler      to emro-08D-filler
      *****     move old-emro-09D-filler      to emro-09D-filler
      *****     move old-emro-10D-filler      to emro-10D-filler
      *****     move old-emro-11D-filler      to emro-11D-filler
      *****     move old-emro-12D-filler      to emro-12D-filler
      *****     move old-emro-02D13-LIN-CODFORTU to emro-02D13-LIN-CODFORTU
      *****     move old-emro-14D-filler      to emro-02D14-LIN-CODDISTU 
      *****     move old-emro-02D15-LIN-DESART to emro-02D15-LIN-DESART
      *****     move old-emro-16D-filler       to emro-16D-filler   
      *****     move old-emro-02D17-QTAORD     to emro-02D17-QTAORD 
      *****     move old-emro-18D-filler       to emro-18D-filler   
      *****     move old-emro-02D19-LIN-PRZUNI to emro-02D19-LIN-PRZUNI
      *****     move old-emro-19D-filler      to emro-19D-filler
      *****     move old-emro-20D-filler      to emro-20D-filler
      *****     move old-emro-21D-filler      to emro-21D-filler
      *****     move old-emro-02D22-LIN-NRCUINTU to emro-02D22-LIN-NRCUINTU
      *****     move old-emro-02D222-QTAPZ    to emro-02D222-QTAPZ  
      *****             
      *****     write emro-rec.
