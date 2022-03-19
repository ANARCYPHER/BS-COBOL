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
              SELECT  ASSIGN TO DISK
              ORGANIZATION SEQUENTIAL
              ACCESS MODE SEQUENTIAL
              FILE STATUS ARQST.

       DATA DIVISION.
       FILE SECTION.
       FD  LABEL RECORD STANDARD
                DATA RECORD IS REG-CLI
                VALUE OF FILE-ID IS "PRODUTOS.DAT".
          01 REG-CLI.
                02 COD         PIC 9(04).
                02 NOME           PIC X(30).
                02 DATANASC       PIC 9(08).
                02 SALDO          PIC 9(05)V99.
                02 TOTAL          PIC 9(06)V99.

       WORKING-STORAGE SECTION.
          01 REG-CLI-E.
                02 COD-E       PIC Z.ZZ9.
                02 NOME-E         PIC X(30).
                02 DATANASC-E     PIC Z.ZZ9.
                02 SALDO-E        PIC ZZ.ZZ9,99.
                02 TOTAL-E        PIC ZZZ.ZZ9,99.
          01 REG-CLI-W.
                02 COD-W         PIC 9(04).
                02 NOME-W           PIC X(30).
                02 DATANASC-W       PIC 9(04).
                02 SALDO-W          PIC 9(05)V99.
                02 TOTAL-W          PIC 9(06)V99.
          01 DATA-SIS.
                02 ANO            PIC 9(04).
                02 MES            PIC 9(02).
                02 DIA            PIC 9(02).

         01 ARQST                   PIC X(02).
         01 WS-OPTION                PIC X(01) VALUE SPACES.
         01 WS-SAVE                PIC X(01) VALUE SPACES.
         01 WS-SPACE               PIC X(30) VALUE SPACES.
         01 WS-MENS1                PIC X(20) VALUE "FIM DE PROGRAMA".
         01 WS-FL                   PIC 9(01) VALUE ZEROS.

         SCREEN SECTION.
         01 MONITOR.
              02 BLANK SCREEN.
              02 LINE 2  COL 5  VALUE "  /  /  ".
              02 COL 29  VALUE "COD of CLIENT".
              02 LINE 4  COL 19 VALUE "ACCOUNT:".
              02 LINE 6  COL 19 VALUE "CLIENT:".
              02 LINE 8  COL 19 VALUE "AGE:".
              02 LINE 10 COL 19 VALUE "ACTUAL BALANCE:".
              02 LINE 12 COL 19 VALUE "TOTAL BALANCE:".
              02 LINE 15 COL 25 VALUE "WARNING:".

       PROCEDURE DIVISION.
       START.
              PERFORM OPEN-ARQ.
              PERFORM PROCESS UNTIL WS-OPTION = "N".
              PERFORM END.
      
       OPEN-ARQ.
              OPEN I-O CLIENTS.
              IF ARQST NOT = "00"
                     CLOSE 
                     OPEN OUTPUT .
       PROCESS.
              PERFORM IMP-MONITOR.
              PERFORM OPEN-DATE.
              PERFORM CAL-TOTAL.
              PERFORM RECORD  UNTIL WS-SAVE = "S" OR "N".
              IF WS-SAVE = "S"
                 PERFORM RECORD-REG
              ELSE
                 DISPLAY "REGISTRY NOT RECORD" AT 2030.
              PERFORM CONTINUED  UNTIL WS-OPTION = "S" OR "N".

       IMP-MONITOR.
              DISPLAY MONITOR.
              MOVE FUNCTION CURRENT-DATE TO DATA-SIS.
              DISPLAY DIA   AT 0205.
              DISPLAY MES   AT 0208.
              DISPLAY ANO   AT 0211.
      * ----------------------------- START
              MOVE SPACE  TO     WS-OPTION
                                 WS-SAVE
                                 NOME-E.
              MOVE ZEROS  TO     COD-E
                                 DATANASC-E
                                 SALDO-E
                                 TOTAL-E
                                 WS-FL.

       OPEN-COD.
              ACCEPT COD-E   AT 0438 WITH PROMPT AUTO.
              MOVE   COD-E   TO COD-W.
              IF COD-W = 9999
                 DISPLAY WS-MENS1 AT 1535
                 CLOSE 
                 STOP RUN.
              CLOSE .
              PERFORM OPEN-ARQ.
              MOVE ZEROS TO WS-FL.
              PERFORM LER-REGISTRY UNTIL WS-FL >= 1.
              IF WS-FL = 2
                 DISPLAY "ALREADY on SYSTEM" AT 2030.

       LER-REGISTRY.
              READ  NEXT AT END MOVE 1 TO WS-FL.
              IF ARQST = "00"
                 IF COD-W = COD
                    MOVE 2 TO WS-FL.      

       CAL-TOTAL.
              COMPUTE TOTAL-W = SALDO-W.
              MOVE    TOTAL-W TO TOTAL-E.
              DISPLAY TOTAL-E AT 1232.

       RECORD.
              DISPLAY "SAVE (S/N)? [ ]" AT 1430.
              ACCEPT WS-SAVE AT 1445 WITH PROMPT AUTO.

       RECORD-REG.
              CLOSE .
              OPEN EXTEND .
              MOVE REG-CLI-W TO REG-CLI.
              WRITE REG-CLI.
              IF ARQST NOT = "00"
                   DISPLAY "ERROR OPS" AT 1535
                   STOP " ".
              CLOSE .
              PERFORM OPEN-ARQ.

       CONTINUED.
              DISPLAY "CONTINUED (S/N)? [ ]" AT 1430.
              ACCEPT WS-OPTION AT 1447 WITH PROMPT AUTO.
              IF WS-OPTION = "S" OR = "N"
                     DISPLAY WS-SPACE AT 1430
                     DISPLAY WS-SPACE AT 1535
              ELSE
                     DISPLAY WS-SPACE AT 1535
                     DISPLAY "WRITE S OU N" AT 1535.
            
       END.
              DISPLAY WS-MENS1 AT 1535.
              CLOSE .
              STOP RUN.      
