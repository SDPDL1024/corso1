000100 IDENTIFICATION DIVISION.
000200 PROGRAM-ID. UDCIL11H.
       DATE-WRITTEN.                    12/05/2010.
000400******************************************************************
000700*****   -DECODIFICA CIFRE-LETTERE                             ****
000700*****   -DECODIFICA CIFRE-LETTERE ANCHE LE ULT. 3 CIFRE       ****
000700*****   -                    DOPO BARRA CENT. DI EURO         ****
000800******************************************************************
000900******************************************************************
001000 ENVIRONMENT DIVISION.
001100 CONFIGURATION SECTION.
001200 SPECIAL-NAMES.
001300     DECIMAL-POINT IS COMMA.
001400 DATA DIVISION.
001500 WORKING-STORAGE SECTION.
001600*****810314*******************************************************
001900 77  WSERV                   PIC S999 COMP-3  VALUE ZERO.
001900 77  IND                     PIC S999 COMP-3  VALUE ZERO.
002000 77  CTR-P                   PIC S999 COMP-3  VALUE ZERO.
       01  WAREA.
260901     03  WIMP                    PIC 9(9)V99 VALUE ZERO.
002200     03  RWIMP   REDEFINES WIMP.
002300         05  WIMP-CMN            PIC 999.
               05  WIMPCML.
002500             07  WIMP-CML        PIC 9.                           DECIFLET
002600             07  WIMP-UML        PIC 99.                          DECIFLET
002700         05  WIMP-CLIR           PIC 999.                         DECIFLET
260901         05  WIMP-DEC            PIC 99.
003000     03  R2WIMP  REDEFINES WIMP.                                  DECIFLET
003100         05  FILLER              PIC 9.                           DECIFLET
003200         05  WIMP-2-7            PIC 99.                          DECIFLET
003300         05  WIMP-6              PIC 9(6)V99.                     DECIFLET
003400     03  WZERO                   PIC  9           VALUE ZERO.     DECIFLET
003500     03  ST-MIL                  PIC  9           VALUE ZERO.     DECIFLET
002800 01  R1WAREA REDEFINES WAREA.                                     DECIFLET
260901     03  WTB-IMP             PIC 9       OCCURS 13 INDEXED IXW.   DECIFLET
003600*                                                                 DECIFLET
003700*****************    PARAMETRI CARICA BUFFER     *****************DECIFLET
003800 01  PRM-LOADB.                                                   DECIFLET
003900     03  PRM01.                                                   DECIFLET
004000         05  P01-ELC         PIC 999     COMP-3  VALUE 1.         DECIFLET
004100         05  P01-ELCC        PIC 9       COMP-3  VALUE 2.         DECIFLET
004200         05  P01-CTRE        PIC 9       COMP-3  VALUE 7.         DECIFLET
004300     03  PRM02.                                                   DECIFLET
004400         05  P02-ELC         PIC 999     COMP-3  VALUE 11.        DECIFLET
004500         05  P02-ELCC        PIC 9       COMP-3  VALUE 1.         DECIFLET
004600         05  P02-CTRE        PIC 9       COMP-3  VALUE 8.         DECIFLET
004700     03  PRM03.                                                   DECIFLET
004800         05  P03-ELC         PIC 999     COMP-3  VALUE 21.        DECIFLET
004900         05  P03-ELCC        PIC 9       COMP-3  VALUE 1.         DECIFLET
005000         05  P03-CTRE        PIC 9       COMP-3  VALUE 8.         DECIFLET
005100     03  PRM04.                                                   DECIFLET
005200         05  P04-ELC         PIC 999     COMP-3  VALUE 1.         DECIFLET
005300         05  P04-ELCC        PIC 9       COMP-3  VALUE 1.         DECIFLET
005400         05  P04-CTRE        PIC 9       COMP-3  VALUE 1.         DECIFLET
005500     03  PRM05.                                                   DECIFLET
005600         05  P05-ELC         PIC 999     COMP-3  VALUE 12.        DECIFLET
005700         05  P05-ELCC        PIC 9       COMP-3  VALUE 4.         DECIFLET
005800         05  P05-CTRE        PIC 9       COMP-3  VALUE 4.         DECIFLET
005900     03  PRM06.                                                   DECIFLET
006000         05  P06-ELC         PIC 999     COMP-3  VALUE 12.        DECIFLET
006100         05  P06-ELCC        PIC 9       COMP-3  VALUE 1.         DECIFLET
006200         05  P06-CTRE        PIC 9       COMP-3  VALUE 2.         DECIFLET
006300     03  PRM07.                                                   DECIFLET
006400         05  P07-ELC         PIC 999     COMP-3  VALUE 31.        DECIFLET
006500         05  P07-ELCC        PIC 9       COMP-3  VALUE 1.         DECIFLET
006600         05  P07-CTRE        PIC 9       COMP-3  VALUE 8.         DECIFLET
006700     03  PRM08.                                                   DECIFLET
006800         05  P08-ELC         PIC 999     COMP-3  VALUE 21.        DECIFLET
006900         05  P08-ELCC        PIC 9       COMP-3  VALUE 1.         DECIFLET
007000         05  P08-CTRE        PIC 9       COMP-3  VALUE 8.         DECIFLET
007100     03  PRM09.                                                   DECIFLET
007200         05  P09-ELC         PIC 999     COMP-3  VALUE 33.        DECIFLET
007300         05  P09-ELCC        PIC 9       COMP-3  VALUE 5.         DECIFLET
007400         05  P09-CTRE        PIC 9       COMP-3  VALUE 2.         DECIFLET
007500     03  PRM10.
007600         05  P09-ELC         PIC 999     COMP-3  VALUE 2.
007700         05  P09-ELCC        PIC 9       COMP-3  VALUE 2.
007800         05  P09-CTRE        PIC 9       COMP-3  VALUE 3.
007500 01  PRMTBLD     REDEFINES PRM-LOADB.                             DECIFLET
007600     03  PRM-TBLD                        OCCURS 10 INDEXED PRM.   DECIFLET
007700         05  PRM-ELC         PIC 999     COMP-3.                  DECIFLET
007800         05  PRM-ELCC        PIC 9       COMP-3.                  DECIFLET
007900         05  PRM-CTRE        PIC 9       COMP-3.                  DECIFLET
008000*                                                                 DECIFLET
008100*****************    COSTANTI LETTERE      ***********************DECIFLET
008200 01  COS-LET.                                                     DECIFLET
008300     03  I-A-00              PIC X(8)  VALUE " DIECI- ".          DECIFLET
008400     03  FILLER              PIC X(8)  VALUE " UNO-   ".          DECIFLET
008500     03  FILLER              PIC X(8)  VALUE "IDUE-   ".          DECIFLET
008600     03  FILLER              PIC X(8)  VALUE "ATRE-   ".          DECIFLET
008700     03  FILLER              PIC X(8)  VALUE "AQUATTRO".          DECIFLET
008800     03  FILLER              PIC X(8)  VALUE "ACINQUE-".          DECIFLET
008900     03  FILLER              PIC X(8)  VALUE "ASEI-   ".          DECIFLET
009000     03  FILLER              PIC X(8)  VALUE "ASETTE- ".          DECIFLET
009100     03  FILLER              PIC X(8)  VALUE "AOTTO-  ".          DECIFLET
009200     03  FILLER              PIC X(8)  VALUE "ANOVE-  ".          DECIFLET
009300     03  CENTO               PIC X(8)  VALUE "CENTO-  ".          DECIFLET
009400     03  DICI                PIC X(8)  VALUE "UN-DICI-".          DECIFLET
009500     03  FILLER              PIC X(8)  VALUE "DO-     ".          DECIFLET
009600     03  FILLER              PIC X(8)  VALUE "TRE-    ".          DECIFLET
009700     03  FILLER              PIC X(8)  VALUE "QUATTOR-".          DECIFLET
009800     03  FILLER              PIC X(8)  VALUE "QUIN-   ".          DECIFLET
009900     03  FILLER              PIC X(8)  VALUE "SE-/-   ".          DECIFLET
010000     03  FILLER              PIC X(8)  VALUE "ASSETTE-".          DECIFLET
010100     03  FILLER              PIC X(8)  VALUE "OTTO-   ".          DECIFLET
010200     03  FILLER              PIC X(8)  VALUE "ANNOVE- ".          DECIFLET
010300     03  CTRP.                                                    DECIFLET
010400         05  FILLER          PIC X     VALUE "/".                 DECIFLET
260901         05  COM-99          PIC 99.                              DECIFLET
260901         05  FILLER          PIC XXXXX  VALUE "*****".            DECIFLET
010700     03  VUOTO               PIC X(8)  VALUE "        ".          DECIFLET
010800     03  DECINX              PIC X(8)  VALUE "VENT-   ".          DECIFLET
010900     03  FILLER              PIC X(8)  VALUE "TRENT-  ".          DECIFLET
011000     03  FILLER              PIC X(8)  VALUE "QUARANT-".          DECIFLET
011100     03  FILLER              PIC X(8)  VALUE "CINQUANT".          DECIFLET
011200     03  FILLER              PIC X(8)  VALUE "SESSANT-".          DECIFLET
011300     03  FILLER              PIC X(8)  VALUE "SETTANT-".          DECIFLET
011400     03  FILLER              PIC X(8)  VALUE "OTTANT- ".          DECIFLET
011500     03  FILLER              PIC X(8)  VALUE "NOVANT- ".          DECIFLET
011600     03  MILION              PIC X(8)  VALUE "MILIONE-".          DECIFLET
011700     03  FILLER              PIC X(8)  VALUE "MILLE-  ".          DECIFLET
011800     03  ASTERX              PIC X(8)  VALUE "UNO-**- ".          DECIFLET
011900     03  FILLER              PIC X(8)  VALUE "MILIONI-".          DECIFLET
012000     03  FILLER              PIC X(8)  VALUE "MILA-   ".          DECIFLET
012100 01  TBCOSLET    REDEFINES COS-LET.                               DECIFLET
012200     03  TBCOS-LET                     OCCURS 35 INDEXED LE8.     DECIFLET
012300         05  TBCOS-ELE       PIC X     OCCURS  8 INDEXED LE1.     DECIFLET
012400*                                                                 DECIFLET
012500 LINKAGE SECTION.                                                 DECIFLET
012600 01  LIMPCIF.                                                     DECIFLET
260901     03  LIMP-CIF            PIC S9(11)V99 COMP-3.                DECIFLET
012800 01  LCAMLET.
110510     03  FILLER              PIC X(100).
110510 01  LCAMLET-R REDEFINES LCAMLET.
012900     03  LCAM-AST            PIC X.                               DECIFLET
013000     03  LTBCAM-LET          PIC X       OCCURS 99   INDEXED LET. DECIFLET
013100 PROCEDURE DIVISION          USING   LIMPCIF  LCAMLET.            DECIFLET
013200 PAR-0.                                                           DECIFLET
013300         MOVE    "*"         TO  LCAMLET.                         DECIFLET
013400         SET     LET         TO  1.                               DECIFLET
013500         MOVE    LIMP-CIF    TO  WIMP                             DECIFLET
013600         MOVE    ZERO        TO  CTR-P   IND ST-MIL.              DECIFLET
013700*                                                                 DECIFLET
260901         SET     IXW         TO  12.                              DECIFLET
013900*                                                                 DECIFLET
014000*****************    ROUTINE DECODIFICA CIFRA/LETTERE    *********DECIFLET
014100 R100-DEC-ROU.                                                    DECIFLET
014200 INIROU.                                                          DECIFLET
014300     IF  WIMP-CMN EQUAL ZERO                                      DECIFLET
014400             GO TO   SHIFT3.                                      DECIFLET
014500         MOVE    CTR-P       TO  ST-MIL.                          DECIFLET
014600     IF  WIMP-CMN EQUAL 1                                         DECIFLET
014700             GO TO   CMP-MIL.                                     DECIFLET
014800     IF  WTB-IMP(1) EQUAL ZERO                                    DECIFLET
014900             GO TO   DUE7-0.                                      DECIFLET
015000     IF  WTB-IMP(1) NOT EQUAL 1                                   DECIFLET
015100         SET     IXW         TO  1                                DECIFLET
015200         SET     PRM         TO  1                                DECIFLET
015300       PERFORM R900-LOA-BUF  THRU R900-EX.                        DECIFLET
015400 DUE7-1.                                                          DECIFLET
015500         SET     PRM         TO  2                                DECIFLET
015600       PERFORM R900-LOA-BUF  THRU R900-EX.                        DECIFLET
015700 DUE7-0.                                                          DECIFLET
015800     IF  WIMP-2-7 EQUAL ZERO                                      DECIFLET
015900             GO TO   MOD-3.                                       DECIFLET
016000     IF  WIMP-2-7 LESS 20                                         DECIFLET
016100             GO TO   MIN11.                                       DECIFLET
016200         SET     IXW         TO  2                                DECIFLET
016300         SET     PRM         TO  3                                DECIFLET
016400       PERFORM R900-LOA-BUF  THRU R900-EX.                        DECIFLET
016500*                                                                 DECIFLET
016600     IF  WTB-IMP(3) EQUAL 1                                       DECIFLET
016700             GO TO   N-UNIT.                                      DECIFLET
016800     IF  WTB-IMP(3) EQUAL 8                                       DECIFLET
016900             GO TO   N-UNIT.                                      DECIFLET
017000         SET     IXW         TO  2                                DECIFLET
017100         SET     PRM         TO  4                                DECIFLET
017200       PERFORM R900-LOA-BUF  THRU R900-EX.                        DECIFLET
017300*                                                                 DECIFLET
017400     IF  WTB-IMP(3) EQUAL ZERO                                    DECIFLET
017500             GO TO   MOD-3.                                       DECIFLET
017600 N-UNIT.                                                          DECIFLET
017700         SET     IXW         TO  3                                DECIFLET
017800         SET     PRM         TO  1                                DECIFLET
017900       PERFORM R900-LOA-BUF  THRU R900-EX                         DECIFLET
018000             GO TO   MOD-3.                                       DECIFLET
018100 MIN11.                                                           DECIFLET
018200     IF  WIMP-2-7 LESS 11                                         DECIFLET
018300             GO TO   N-UNIT.                                      DECIFLET
018400     IF  WTB-IMP(3) NOT LESS 7                                    DECIFLET
018500         SET     PRM         TO  5                                DECIFLET
018600       PERFORM R900-LOA-BUF  THRU R900-EX.                        DECIFLET
018700 K-DICIA1.                                                        DECIFLET
018800         SET     IXW         TO  3                                DECIFLET
018900         SET     PRM         TO  2                                DECIFLET
019000       PERFORM R900-LOA-BUF  THRU R900-EX.                        DECIFLET
019100*                                                                 DECIFLET
019200     IF  WTB-IMP(3) NOT GREATER 6                                 DECIFLET
019300         SET     PRM         TO  5                                DECIFLET
019400       PERFORM R900-LOA-BUF  THRU R900-EX.                        DECIFLET
019500*                                                                 DECIFLET
019600 MOD-3.                                                           DECIFLET
019700         ADD     3           TO  ST-MIL.                          DECIFLET
019800 CMP-MIL.                                                         DECIFLET
019900     IF  ST-MIL EQUAL ZERO                                        DECIFLET
020000         SET     PRM         TO  6                                DECIFLET
020100       PERFORM R900-LOA-BUF  THRU R900-EX.                        DECIFLET
020200*                                                                 DECIFLET
020300 STAMIL.                                                          DECIFLET
260901         SET     IXW         TO  13                               DECIFLET
020500         SET     PRM         TO  7                                DECIFLET
020600       PERFORM R900-LOA-BUF  THRU R900-EX.                        DECIFLET
020700 SHIFT3.                                                          DECIFLET
020800         MULTIPLY WIMP-6 BY 1000   GIVING WIMP                    DECIFLET
020900         ADD     1           TO  CTR-P.                           DECIFLET
021000     IF  CTR-P NOT EQUAL 2                                        DECIFLET
021100             GO TO   INIROU.                                      DECIFLET
021600*
021700**-----> DECOD. LE ULT. 3 CIFRE
021800     IF  WIMP-CMN EQUAL 1
021900         SET     PRM         TO  10
022000       PERFORM   R900-LOA-BUF THRU  R900-EX
022100             GO TO  3C-STAMPA.
022200     IF  WTB-IMP(1) EQUAL ZERO
022300             GO TO  3C-DUE7-0.
022400     IF  WTB-IMP(1) NOT EQUAL 1
022500         SET     IXW         TO  1
022600         SET     PRM         TO  1
022700       PERFORM R900-LOA-BUF  THRU R900-EX.
022800 3C-DUE7-1.
022900         SET     PRM         TO  2
023000       PERFORM R900-LOA-BUF  THRU R900-EX.
023100 3C-DUE7-0.
023200     IF  WIMP-2-7 EQUAL ZERO
023300             GO TO  3C-STAMPA.
023400     IF  WIMP-2-7 LESS 20
023500             GO TO  3C-MIN11.
023600         SET     IXW         TO  2
023700         SET     PRM         TO  3
023800       PERFORM R900-LOA-BUF  THRU R900-EX.
023900*
024000     IF  WTB-IMP(3) EQUAL 1
024100             GO TO  3C-N-UNIT.
024200     IF  WTB-IMP(3) EQUAL 8
024300             GO TO  3C-N-UNIT.
024400         SET     IXW         TO  2
024500         SET     PRM         TO  4
024600       PERFORM R900-LOA-BUF  THRU R900-EX.
024700*
024800     IF  WTB-IMP(3) EQUAL ZERO
024900             GO TO  3C-STAMPA.
025000 3C-N-UNIT.
025100         SET     IXW         TO  3
025200         SET     PRM         TO  1
025300       PERFORM R900-LOA-BUF  THRU R900-EX
025400             GO TO  3C-STAMPA.
025500 3C-MIN11.
025600     IF  WIMP-2-7 LESS 11
025700             GO TO   3C-N-UNIT.
025800     IF  WTB-IMP(3) NOT LESS 7
025900         SET     PRM         TO  5
026000       PERFORM R900-LOA-BUF  THRU R900-EX.
026100 3C-K-DICIA1.
026200         SET     IXW         TO  3
026300         SET     PRM         TO  2
026400       PERFORM R900-LOA-BUF  THRU R900-EX.
026500*
026600     IF  WTB-IMP(3) NOT GREATER 6
026700         SET     PRM         TO  5
026800       PERFORM R900-LOA-BUF  THRU R900-EX.
026900*
027000 3C-STAMPA.
      *        MULTIPLY WIMP-6 BY 1000   GIVING WIMP                    DECIFLET
260901         MULTIPLY WIMP-6 BY 100    GIVING WIMP                    DECIFLET
260901*    IF  WIMP-DEC  NOT EQUAL ZERO                                 DECIFLET
260901     IF  WIMP-CMN  NOT EQUAL ZERO                                 DECIFLET
               MOVE    WIMP-CMN    TO  WIMP-DEC                         DECIFLET
260901         MOVE    WIMP-DEC    TO  COM-99                           DECIFLET
260901         SET     PRM         TO  8                                DECIFLET
260901       PERFORM R900-LOA-BUF  THRU R900-EX.                        DECIFLET
021600*                                                                 DECIFLET
021700         SET     PRM         TO  9                                DECIFLET
021800       PERFORM R900-LOA-BUF  THRU R900-EX.                        DECIFLET
021900*                                                                 DECIFLET
022000 EXIT-PRO.                                                        DECIFLET
022100 USCITA.                                                          DECIFLET
022200       EXIT  PROGRAM.                                             DECIFLET
022300*                                                                 DECIFLET
022400*****************    CARICA  BUFFER              *****************DECIFLET
022500 R900-LOA-BUF.                                                    DECIFLET
022600         MOVE  WTB-IMP(IXW)  TO  IND                              DECIFLET
022700         SET     LE8         TO  PRM-ELC(PRM)                     DECIFLET
022800         SET     LE8         UP BY IND                            DECIFLET
022900         MOVE PRM-ELCC(PRM)  TO  IND                              DECIFLET
023000         SET     LE1         TO  IND                              DECIFLET
023100         SET     IXW         TO  PRM-CTRE(PRM)                    DECIFLET
023200         SET     IXW         UP BY IND.                           DECIFLET
023300 R900-010.                                                        DECIFLET
023400         MOVE TBCOS-ELE(LE8, LE1) TO LTBCAM-LET(LET).             DECIFLET
023500     IF  LTBCAM-LET(LET) EQUAL "-"                                DECIFLET
023600             GO TO   R900-020.                                    DECIFLET
023700         SET     LET LE1     UP BY 1.                             DECIFLET
023800     IF  LE1 LESS IXW                                             DECIFLET
023900             GO TO R900-010.                                      DECIFLET
024000 R900-020.                                                        DECIFLET
260901         SET     IXW         TO  12.                              DECIFLET
024200 R900-EX. EXIT.                                                   DECIFLET
024300*******************FINE PROGRAMMA*********************************DECIFLET
