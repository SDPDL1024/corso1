       IDENTIFICATION DIVISION.
       PROGRAM-ID. UDATE10X.
       DATE-WRITTEN. 28.05.99.
modificato da git
      ********************************************************
nuova modif su master
      ***  FUNZIONI:   CONTROLLO DI VALIDITA' DELLA  DATA   
      ***                                                   
      ***                  I N P U T      O U T P U T   
      ***                                                   
      ***   SWDATA = 0 :  LD = GGMMAAAA   LD = GGMMAAAA 
      ***            1 :  LD = AAAAMMGG   LD = GGMMAAAA 
      ***            2 :  LD = AAAAMMGG   LD = AAAAMMGG       
      ***            3 :  LD = GGMMAAAA   LD = AAAAMMGG       
      ***            / :  LD =            LD =                
      ***            / :  LD =            LD =                
      ***            / :  LD =            LD =                
      ***            / :  LD =            LD =                
      ***            8 :  LD = IPL        LD = AAAAMMGG       
      ***            9 :  LD = IPL        LD = GGMMAAAA       
      ********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
      *{TOTEM}ACTIVEX-DEF
      *{TOTEM}END
      *{Bench}activex-def
      *{Bench}end
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *{TOTEM}FILE-CONTROL
      *{TOTEM}END
      *{Bench}file-control
      *{Bench}end
       DATA DIVISION.
       FILE SECTION.
      *{TOTEM}FILE
      *{TOTEM}END
      *{Bench}file
      *{Bench}end
       WORKING-STORAGE SECTION.
      *{Bench}acu-def
      *{Bench}end
       77  WS-NOME-PROG             PIC X(8)    VALUE "Udate10x".
       77  SW6                     PIC 9       COMP-3  VALUE ZERO.
       01  WCOM.
           03  W-COM               PIC 9999    COMP-3.
           03  WRESTO              PIC 9       COMP-3.
       01  WDATEIPL.
           03  FILLER              PIC X VALUE SPACE.
           03  WDATE-IPL-AAAAMMGG-HHMM.
               05  WDATE-IPL-AAAAMMGG.
                   07  WDATE-IPL-AA    PIC 9999.
                   07  WDATE-IPL-MM    PIC 99.
                   07  WDATE-IPL-GG    PIC 99.
               05  WHORA-IPL-YYYYMMAA  PIC X(13).

       01  WDATA.
           03  SWDATA              PIC 9       COMP-3.
               88  INVA-G                      VALUE 1, 2, 8, 9.
               88  INVO-AMG                    VALUE 3.
               88  INVO-GMA                    VALUE 1, 9.
           03  WDATAS.
               05  WGG             PIC 9999    COMP-3.
                   88  GG-OK                   VALUE 01 THRU 31.
               05  WMM             PIC 9999    COMP-3.
                   88  MM-OK                   VALUE 1 THRU 12.
                   88  MM-30                   VALUE 4, 6, 9, 11.
               05  WAAAA           PIC 9999    COMP-3.
      *
      *{TOTEM}COPY-WORKING
      *{TOTEM}END
      *{Bench}copy-working
      *{Bench}end
       LINKAGE SECTION.
       01  LDATA.
           03  SLDATA              PIC 9.
           03  LD.
               05  FILLER          PIC X.
               05  LGG             PIC 99.
               05  LMM             PIC 99.
               05  LAAAA           PIC 9999.
           03  LD-R  REDEFINES LD.
               05  FILLER          PIC X.
               05  LAAAA-A         PIC 9999.
               05  LMM-A           PIC 99.
               05  LGG-A           PIC 99.
160510     03  LD-R1  REDEFINES LD.
               05  FILLER          PIC X.
               05  L-XXXXXX        PIC 9(8).
160510     03  LD-R2  REDEFINES LD.
               05  FILLER          PIC XXX.
               05  L-GGMMAA.
                   07 L-GG2        PIC 99.
                   07 L-MM2        PIC 99.
                   07 L-AA2        PIC 99.
      *{TOTEM}LINKAGE
      *{TOTEM}END
      *{Bench}linkage
      *{Bench}end
       SCREEN SECTION.
      *{TOTEM}COPY-SCREEN
      *{TOTEM}END
      *{Bench}copy-screen
      *{Bench}end
       PROCEDURE DIVISION  USING  LDATA.
       VIA.
               MOVE    SLDATA      TO  SWDATA.
           IF  SWDATA NOT GREATER 7
                   GO TO EXINV.
             MOVE FUNCTION  CURRENT-DATE TO  WDATE-IPL-AAAAMMGG-HHMM
               MOVE    WDATE-IPL-AA TO  LAAAA
               MOVE    WDATE-IPL-GG TO  LGG
               MOVE    WDATE-IPL-MM TO  LMM.
       EXINV.
160510*     IF  L-XXXXXX LESS 1000000
   "  *         ADD     2000        TO  L-AA2 GIVING WDATE-IPL-AA WAAAA
      *         MOVE    L-GG2       TO  WDATE-IPL-GG WGG
      *         MOVE    L-MM2       TO  WDATE-IPL-MM WMM
      *                                             ELSE
               MOVE    LGG         TO  WDATE-IPL-GG WGG
               MOVE    LMM         TO  WDATE-IPL-MM WMM
               MOVE    LAAAA       TO  WDATE-IPL-AA WAAAA
      *     END-IF
               MOVE    ZERO        TO  SLDATA.
           IF  NOT INVA-G  GO TO   EXINV-1.
               MOVE    LGG-A       TO  WDATE-IPL-GG WGG
               MOVE    LMM-A       TO  WDATE-IPL-MM WMM
               MOVE    LAAAA-A     TO  WDATE-IPL-AA WAAAA.
       EXINV-1.
           IF  LDATA   EQUAL  ZEROES
               GO TO    EXDATA.
           IF  NOT GG-OK
                   GO  TO  ERR.
           IF  NOT MM-OK
                   GO  TO  ERR.
           IF  WDATE-IPL-MM NOT EQUAL 02
                   GO TO  ALTRIMESI.
               DIVIDE  4  INTO WDATE-IPL-AA
                GIVING W-COM REMAINDER  WRESTO.
           IF  WRESTO EQUAL ZERO
                   GO TO BISESTILE.
           IF  WDATE-IPL-GG LESS 29
                   GO TO  EXDATA.
                   GO TO  ERR.
       BISESTILE.
           IF  WDATE-IPL-GG LESS 30
                   GO TO  EXDATA.
                   GO TO  ERR.
       ALTRIMESI.
           IF  NOT MM-30   GO TO EXDATA.
           IF  WDATE-IPL-GG LESS 31 GO TO EXDATA.
       ERR.
               MOVE    1           TO  SLDATA.
       EXDATA.
           IF  INVO-AMG
               MOVE    WDATE-IPL-AA    TO  LAAAA-A
               MOVE    WDATE-IPL-MM    TO  LMM-A
               MOVE    WDATE-IPL-GG    TO  LGG-A
                   GO  TO  EXIT-PRO.
           IF  NOT INVO-GMA
                   GO  TO  EXIT-PRO.
               MOVE    WDATE-IPL-GG    TO  LGG
               MOVE    WDATE-IPL-MM    TO  LMM
               MOVE    WDATE-IPL-AA    TO  LAAAA.
       EXIT-PRO.
           EXIT PROGRAM.
      *{TOTEM}COPY-PROCEDURE
      *{TOTEM}END
      *{Bench}copy-procedure
      *{Bench}end
