    ****************************************************** ****************
      * Author: ALLSAFECYBER
      * Gives you:
      * Purpose: Banking C.R.U.D
      * Tectonics: cobc
      ****************************************************** ****************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CLIENT.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
              SELECT CLIENTS ASSIGN TO DISK
              SEQUENTIAL ORGANIZATION
              SEQUENTIAL ACCESS MODE
              FILE STATUS ARQST.

       DATE DIVISION.
       FILE SECTION.
       FD CLIENTS LABEL RECORD STANDARD
                DATA RECORD IS REG-CLI
                VALUE OF FILE-ID IS "PRODUCTS.DAT".
          01 REG-CLI.
                02 CODE PIC 9(04).
                02 PIC NAME X(30).
                02 DATANASC PIC 9(04).
                02 PIC 9(05)V99 BALANCE.
                02 TOTAL PIC 9(06)V99.

       WORKING-STORAGE SECTION.
          01 REG-CLI-E.
                02 E-CODE PIC Z.ZZ9.
                02 E-NAME PIC X(30).
                02 DATANASC-E PIC Z.ZZ9.
                02 BALANCE-E PIC ZZ.ZZ9.99.
                02 TOTAL-E PIC ZZZ.ZZ9.99.
          01 REG-CLI-W.
                02 CODE-W PIC 9(04).
                02 NAME-W PIC X(30).
                02 DATANASC-W PIC 9(04).
                02 BALANCE-W PIC 9(05)V99.
                02 TOTAL-W PIC 9(06)V99.
          01 DATA-SIS.
                02 YEAR PIC 9(04).
                02 MONTH PIC 9(02).
                02 DAY PIC 9(02).

         01 ARQST PIC X(02).
         01 WS-OPCAO PIC X(01) VALUE SPACES.
         01 WS-SAVE PIC X(01) VALUE SPACES.
         01 WS-ESPACO PIC X(30) VALUE SPACES.
         01 WS-MENS1 PIC X(20) VALUE "END OF PROGRAM".
         01 WS-FL PIC 9(01) VALUE ZEROS.
      
       SCREEN SECTION.
         01 SCREEN
              02 BLANK SCREEN.
              02 LINE 2 COL 5 VALUE " / / ".
              02 COL 29 VALUE "BANKING SYSTEM ED.FILES".
              02 LINE 4 COL 19 VALUE "ACCOUNT CODE:".
              02 LINE 6 COL 19 VALUE "OWNER'S NAME:".
              02 LINE 7 COL 19 VALUE "NEW NAME OF THE OWNER:".
              02 LINE 8 COL 19 VALUE "AGE:".
              02 LINE 9 COL 19 VALUE "NEW AGE:".
              02 LINE 10 COL 19 VALUE "CURRENT BALANCE:".
              02 LINE 11 COL 19 VALUE "NEW CURRENT BALANCE:".
              02 LINE 12 COL 19 VALUE "TOTAL COST:".
              02 LINE 15 COL 25 VALUE "MESSAGE:".


       PROCEDURE DIVISION.
       START.
              PERFORM OPEN-FILE
              PERFORM PROCESS UNTIL WS-OPTION = "N".
              PERFORM FINALIZES.

       OPEN-FILE
              OPEN I-O CUSTOMERS.
              IF ARQST NOT = "00"
                     CLOSE CUSTOMERS
                     OPEN OUTPUT CLIENTS.

       PROCESS.
              PERFORM PRINT-SCREEN.
              PERFORM DATA INPUT.
              PERFORM DATA SHOW.
              PERFORM ENTER-NEW.
              PERFORM TOTAL CALCULATION.
              PERFORM WRITE UNTIL WS-SALVA = "S" OR "N".
              IF WS-SAVE = "S"
                 PERFORM RECORD
              ELSE
                 DISPLAY "RECORD NOT RECORDED" AT 2030.
              PERFORM CONTINUES UNTIL WS-OPTION = "Y" OR "N".

       IMP-SCREEN.
              DISPLAY SCREEN.
              MOVE FUNCTION CURRENT-DATE TO DATA-SIS.
              DISPLAY DAY AT 0205.
              DISPLAY MES AT 0208.
              DISPLAY YEAR AT 0211.
      * ----------------------------- Variable initialization
              MOVE SPACE TO WS-OPTION
                                 WS-SAVE
                                 NAME IS.
              MOVE ZEROS TO E-CODE
                                 DATANASC-E
                                 BALANCE-E
                                 TOTAL-E
                                 WS-FL.
      
       DATA ENTRY.
              PERFORM CODE ENTER UNTIL WS-FL = 1.
              DISPLAY WS-ESPACO AT 2030.
              MOVE CODE-W TO CODE-E.
              MOVE NAME-W TO NAME-E.
              MOVE DATANASC-W TO DATANASC-E.
              MOVE BALANCE-W TO BALANCE-E.
              MOVE TOTAL-W TOTAL-E.

       DATA SHOW.
           DISPLAY NAME-E AT 0636.
           DISPLAY DATANASC-E AT 0831.
           DISPLAY BALANCE-E AT 1035.
           DISPLAY TOTAL-E AT 1232.


       TO RECORD.
              DISPLAY "SAVE (Y/N)? [ ]" AT 1430.
              ACCEPT WS-SALVA AT 1445 WITH AUTO PROMPT.


       RECORD
              CLOSE CUSTOMERS.
              OPEN I-O CUSTOMERS.
              READ CUSTOMERS.
              MOVE REG-CLI-W TO REG-CLI.
              REWRITE REG