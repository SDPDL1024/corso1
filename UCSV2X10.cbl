       IDENTIFICATION DIVISION.
       PROGRAM-ID. UCSV2X10.
       DATE-WRITTEN.  25/02/2013.    
      ******************************************************************
      *****       FUNZIONE DEL PROGRAMMA:
      *****  L-TIPO-INPU :
      ****
      ***** 1)  - CONVERTE UNA STRINGA FORMATO CSV IN FORMATO ASCII  
      *****      INPUT : L-AREACSV E' LA STRINGA CSV CON ";"
      ****       es : 1;FATTURA;01/01/2013
      *****      OUTPUT  L-AREASCII E' L'AREA CON I CAMPI (MAX 20) 
      *****         ALLINEATI A SX. OGNI CAMPO E F.TO DA 50 CHAR 
      *****      es : 1                    FATTURA            01/01/2013
      ***** 2)   - CONVERTE UNA STRINGA FORMATO ASCII IN FORMATO CSV
      *****      INPUT : L-AREASCII E' LA STRINGA ASCII CON 20 POSIZIONI
      *****              DI 50 CRT (20 RIGHE E 50 COL). I CAMPI DEVONO
      *****              ESSERE ALLINEATI A SX
      *****      OUPUT : L-AREACSV E' LA STRINGA CSV CON ; (MAX  20)
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  I-CSV                       PIC S999  COMP-3 VALUE 0.
       77  I-ASC                       PIC S9(5) COMP-3 VALUE 0.
       77  I-IND                       PIC S9(5) COMP-3 VALUE 0.
       77  I-CTR                       PIC S999  COMP-3 VALUE 0.
       77  I-RIG                       PIC S999  COMP-3 VALUE 0.
      *
       LINKAGE SECTION.
       01  L-AREACSV.
           03  L-RIGA-CSV              PIC X OCCURS 500.
080513     03  L-TIPO-CONV             PIC 9.
       01  L-AREASCII.
           03  L-RIGA-ASCI             PIC X OCCURS 1000.
080513 01  L-AREAASCII-R REDEFINES L-AREASCII.
   "       03  L-MATR-ASCI             OCCURS 20.
   "           05  L-COLN-ASCI         PIC X  OCCURS 50.
       PROCEDURE DIVISION              USING L-AREACSV L-AREASCII.
       ENTRYS.

090513     EVALUATE  L-TIPO-CONV
090513       WHEN 2
090513         MOVE    SPACES          TO  L-AREACSV
090513        PERFORM  R200-LAV
090513       WHEN OTHER
               MOVE    SPACES          TO  L-AREASCII
              PERFORM  R100-LAV  
           END-EVALUATE
             .
       EXIT-PRO.
           EXIT PROGRAM.
      ******************************************************************
      *****    CONVERSIONE FILE  DA CSV  -> ASCII                   ****
       R100-LAV.
      *---> devono essere presenti max 50 separatori ;
      *---> ogni separatore e' un campo di 50 crt
             MOVE      1               TO  I-IND I-ASC 
             MOVE      0               TO  I-CTR 
           PERFORM VARYING I-CSV  FROM 1 BY 1 UNTIL I-CSV GREATER 500
             IF  L-RIGA-CSV(I-CSV) EQUAL ";"
231015*per file Dicanio a Margherita C.
231015                                    OR EQUAL "|"
                 MOVE  0               TO  I-CTR
                 ADD   50              TO  I-IND
              IF  I-IND GREATER 1000
                 MOVE  501             TO  I-CSV
              END-IF
                 MOVE  I-IND           TO  I-ASC
                                                     ELSE
                 ADD   1               TO  I-CTR
              IF  I-CTR NOT GREATER 50
                 MOVE  L-RIGA-CSV(I-CSV)   TO  L-RIGA-ASCI(I-ASC)
                 ADD   1               TO  I-ASC
              END-IF
             END-IF
           END-PERFORM
           .
      ******************************************************************
      *****    CONVERSIONE FILE  DA ASCII A CSV                   ****
       R200-LAV.
             MOVE      0               TO  I-CSV
           PERFORM VARYING I-RIG  FROM 1 BY 1 UNTIL I-RIG GREATER 20

               MOVE      50              TO  I-ASC
            PERFORM UNTIL I-ASC GREATER 0

              IF  L-COLN-ASCI(I-RIG,I-ASC) EQUAL SPACES
               SUBTRACT 1               FROM  I-ASC  ELSE
               MOVE     I-ASC           TO  I-CTR
               MOVE     0               TO  I-ASC
              END-IF
            END-PERFORM
            IF  I-CTR GREATER 0
             PERFORM VARYING I-ASC FROM 1 BY 1 UNTIL I-ASC EQUAL I-CTR
               ADD     1               TO  I-CSV
              MOVE      L-COLN-ASCI(I-RIG,I-ASC) TO  L-RIGA-CSV(I-CSV)
             END-PERFORM
             ADD       1               TO  I-CSV
             MOVE      ";"             TO  L-RIGA-CSV(I-CSV)
            END-IF
           END-PERFORM 
           .


