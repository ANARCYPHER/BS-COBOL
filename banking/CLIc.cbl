      ******************************************************************
      * Author: ALLSAFECYBER
      * Date: 
      * Purpose: Banking C.R.U.D
      * Tectonics: cobc
      ******************************************************************

       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CLIENT.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.        DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
              SELECT CLIENT ASSIGN TO DISK
              ORGANIZATION SEQUENTIAL
              ACCESS MODE SEQUENTIAL
              FILE STATUS ARQST.

       DATA DIVISION.
       FILE SECTION.
       FD CLIENT LABEL RECORD STANDARD
                DATA RECORD IS REG-CLI
                VALUE OF FILE-ID IS "PRODUCTS.DAT".
          01 REG-CLI.
                02 DIGIT         PIC 9(04).
                02 NAME           PIC X(30).
                02 DATANASC       PIC 9(04).
                02 BALANCE          PIC 9(05)V99.
                02 TOTAL          PIC 9(06)V99.

       WORKING-STORAGE SECTION.
          01 REG-CLI-E.
                02 DIGIT-E       PIC Z.ZZ9.
                02 NAME-E         PIC X(30).
                02 DATANASC-E       PIC Z.ZZ9.
                02 BALANCE-E     PIC ZZ.ZZ9,99.
                02 TOTAL-E        PIC ZZZ.ZZ9,99.
          01 REG-CLI-W.
                02 DIGIT-W         PIC 9(04).
                02 NAME-W           PIC X(30).
                02 DATANASC-W         PIC 9(04).
                02 BALANCE-W       PIC 9(05)V99.
                02 TOTAL-W          PIC 9(06)V99.
          01 DATA-SIS.
                02 ANO            PIC 9(04).
                02 MES            PIC 9(02).
                02 DIA            PIC 9(02).

         01 ARQST                   PIC X(02).
         01 WS-OPTION                PIC X(01) VALUE SPACES.
         01 WS-SAVE                PIC X(01) VALUE SPACES.
         01 WS-SPACE               PIC X(30) VALUE SPACES.
         01 WS-MENS1                PIC X(20) VALUE "FIM DE PROGRAM".
         01 WS-FL                   PIC 9(01) VALUE ZEROS.

       SCREEN SECTION.
         01 MONITOR.
              02 BLANK SCREEN.
              02 LINE 2  COL 5  VALUE "  /  /  ".
              02 COL 29  VALUE "BANKING CONTROL".
              02 LINE 4  COL 19 VALUE "DIGIT of ACCOUNT:".
              02 LINE 6  COL 19 VALUE "NAME DO OWNER:".
              02 LINE 8  COL 19 VALUE "AGE:".
              02 LINE 10 COL 19 VALUE "BALANCE:".
              02 LINE 12 COL 19 VALUE "BALANCE TOTAL:".
              02 LINE 15 COL 25 VALUE "WARNING:".
      
       PROCEDURE DIVISION.
       START.
              PERFORM OPEN-ARQ.
              PERFORM PROCESS UNTIL WS-OPTION = "N".
              PERFORM END.

       OPEN-ARQ.
              OPEN I-O CLIENT.
              IF ARQST NOT = "00"
                     CLOSE CLIENT
                     OPEN OUTPUT CLIENT.

       PROCESS.
              PERFORM IMP-MONITOR.
              PERFORM GO-DADOS.
              PERFORM SHOW-DADOS.
              PERFORM CONTINUA  UNTIL WS-OPTION = "S" OR "N".

       IMP-MONITOR.
              DISPLAY MONITOR.
              MOVE FUNCTION CURRENT-DATE TO DATA-SIS.
              DISPLAY DIA   AT 0205.
              DISPLAY MES   AT 0208.
              DISPLAY ANO   AT 0211.
      * ----------------------------- START
              MOVE SPACE  TO     WS-OPTION
                                 WS-SAVE
                                 NAME-E.
              MOVE ZEROS  TO     DIGIT-E
                                 DATANASC-E
                                 BALANCE-E
                                 TOTAL-E
                                 WS-FL.
       GO-DADOS.
              PERFORM IMP-MONITOR.
              PERFORM GO-DIGIT UNTIL WS-FL = 1.
              DISPLAY WS-SPACE AT 2030.
              MOVE   DIGIT-W   TO DIGIT-E.
              MOVE   NAME-W     TO NAME-E.
              MOVE   DATANASC-W   TO DATANASC-E.
              MOVE   BALANCE-W TO BALANCE-E.
              MOVE   TOTAL-W TO TOTAL-E.

       SHOW-DADOS.
           DISPLAY NAME-E     AT 0636.
           DISPLAY DATANASC-E AT 0831.
           DISPLAY BALANCE-E    AT 1035.
           DISPLAY TOTAL-E    AT 1232.




       GO-DIGIT.
              ACCEPT DIGIT-E   AT 0438 WITH PROMPT AUTO.
              MOVE   DIGIT-E   TO DIGIT-W.
              IF DIGIT-W = 9999
                 DISPLAY WS-MENS1 AT 1535
                 CLOSE CLIENT
                 STOP RUN.
              CLOSE CLIENT.
              PERFORM OPEN-ARQ.
              MOVE ZEROS TO WS-FL.
              PERFORM LER-REGISTRY UNTIL WS-FL >= 1.
              IF WS-FL = 2
                 DISPLAY "NO REGISTRY" AT 2030.

       LER-REGISTRY.
              READ CLIENT NEXT AT END MOVE 2 TO WS-FL.
              IF ARQST = "00"
                 IF DIGIT-W = DIGIT
                    MOVE REG-CLI TO REG-CLI-W
                    MOVE 1 TO WS-FL.

       CONTINUA.
              DISPLAY "CONTINUA (S/N)? [ ]" AT 1430.
              ACCEPT WS-OPTION AT 1447 WITH PROMPT AUTO.
              IF WS-OPTION = "S" OR = "N"
                     DISPLAY WS-SPACE AT 1430
                     DISPLAY WS-SPACE AT 1535
              ELSE
                     DISPLAY WS-SPACE AT 1535
                     DISPLAY "WRITE S OU N" AT 1535.

       END.
              DISPLAY WS-MENS1 AT 1535.
              CLOSE CLIENT.
              STOP RUN.
      
