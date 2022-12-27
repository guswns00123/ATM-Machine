      ******************************************************************
      *
      * CSCI3180 Principles of Programming Languages
      *
      * --- Declaration ---
      *
      * I declare that the assignment here submitted is original except for source
      * material explicitly acknowledged. I also acknowledge that I am aware of
      * University policy and regulations on honesty in academic work, and of the
      * disciplinary guidelines and procedures applicable to breaches of such policy
      * and regulations, as contained in the website
      * http://www.cuhk.edu.hk/policy/academichonesty/
      *
      * Assignment 1
      * Name : Yoo Hyun Jun
      * Student ID : 1155100531
      * Email Addr : hjyoo8@cse.cuhk.edu.hk
      *
      ******************************************************************
       IDENTIFICATION DIVISION.

       PROGRAM-ID. ATM.
       ENVIRONMENT DIVISION.
           INPUT-OUTPUT SECTION.
           FILE-CONTROL.
               SELECT OPTIONAL MASTER ASSIGN TO "master.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

               SELECT ATM-711 ASSIGN TO "trans711.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

               SELECT ATM-713 ASSIGN TO "trans713.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
           FILE SECTION.
           FD MASTER.
           01 WIZARD-INFO.
               03 ACC-NAME PIC A(20).
               03 ACC-NUM PIC X(16).
               03 ACC-PWD PIC 9(6).
               03 ACC-BLC PIC 9(16).

           FD ATM-711.
           01 UPD-INFO.
               03 UPD-NUM PIC 9(16).
               03 OPER PIC A(1).
               03 UPD-AMOUNT PIC 9(7).
               03 TIME-STAMP-711 PIC 9(5).

           FD ATM-713.
           01 UPD2-INFO.
               03 UPD2-NUM PIC 9(16).
               03 OPER2 PIC A(1).
               03 UPD2-AMOUNT PIC 9(7).
               03 TIME-STAMP-713 PIC 9(5).


       WORKING-STORAGE SECTION.
       77 ATM_NUM PIC 9(5).
       77 ACC PIC X(20).
       77 ACCBLC PIC 9(15).
       77 T-ACC PIC X(20).
       77 PWD PIC 9(6).
       01 WZ-INFO.
           03 WZ-NAME PIC A(20).
           03 WZ-NUM PIC X(16).
           03 WZ-PWD PIC 9(6).
           03 WZ-NEG PIC A(1).
           03 WZ-BLC PIC 9(15).
       01 WS-EOF PIC A(1).
       77 C PIC A(1).
       77 AMOUNT PIC S9(13)V9(2).
       77 ASK PIC A(1).
       77 TIME-STAMP PIC 9(5).


       PROCEDURE DIVISION.
       START-PROCEDURE.
           OPEN OUTPUT ATM-711.
           OPEN OUTPUT ATM-713.
           DISPLAY "##############################################".
           DISPLAY "##         Gringotts Wizrding Bank          ##"
           DISPLAY "##                 Welcome                  ##"
           DISPLAY "##############################################".
           GO TO CHOOSE-ATM.

       CHOOSE-ATM.
           DISPLAY "=> PLEASE CHOOSE THE ATM".
           DISPLAY "=> PRESS 1 FOR ATM 711".
           DISPLAY "=> PRESS 2 FOR ATM 713".
           ACCEPT ATM_NUM FROM CONSOLE.
           IF ATM_NUM = 1 THEN GO TO INPUT-ACCOUNT.

           IF ATM_NUM = 2 THEN GO TO INPUT-ACCOUNT.

           IF ATM_NUM >= 3 OR ATM_NUM <= 0 THEN
               DISPLAY "=> INVALID INPUT",
               GO TO CHOOSE-ATM.

       INPUT-ACCOUNT.
           DISPLAY "=> ACCOUNT".
           ACCEPT ACC FROM CONSOLE.
           DISPLAY "=> PWD".
           ACCEPT PWD FROM CONSOLE.
           GO TO OPEN-MASTER.

       OPEN-MASTER.
           OPEN INPUT MASTER.
           GO TO READ-FILE.

       READ-FILE.
           READ MASTER INTO WZ-INFO
               AT END GO TO RE-INPUT.
           GO TO CHECK.

       CHECK.
           IF ACC NOT = WZ-NUM OR PWD NOT = WZ-PWD
              THEN GO TO READ-FILE.
           IF PWD = WZ-PWD AND ACC = WZ-NUM THEN GO TO CHOOSE-SERVICE.

       RE-INPUT.
           CLOSE MASTER.
           DISPLAY "=> INCORRECT ACCOUNT/PASSWORD".
           DISPLAY "=> ACCOUNT".
           ACCEPT ACC FROM CONSOLE.
           DISPLAY "=> PWD".
           ACCEPT PWD FROM CONSOLE.
           GO TO OPEN-MASTER.


       CHOOSE-SERVICE.
           CLOSE MASTER.
           MOVE WZ-BLC TO ACCBLC.
           IF WZ-NEG ='-' THEN
           DISPLAY "=> NEGATIVE REMAINS TRANSACTION ABORT",
           GO TO INPUT-ACCOUNT.

           DISPLAY "=> PLEASE CHOOSE YOUR SERVICE".
           DISPLAY "=> PRESS D FOR DEPOSIT".
           DISPLAY "=> PRESS W FOR WITHDRAWAL".
           DISPLAY "=> PRESS T FOR TRANSFER".
           ACCEPT C FROM CONSOLE.
           IF C = 'D' THEN GO TO D.
           IF C = 'W' THEN GO TO W.
           IF C = 'T' THEN GO TO T.
           IF C NOT = 'D' OR NOT ='W' OR NOT = 'T' THEN
               DISPLAY "=> INVALID INPUT", GO TO RE-CHOOSE.

       RE-CHOOSE.
           DISPLAY "=> PLEASE CHOOSE YOUR SERVICE".
           DISPLAY "=> PRESS D FOR DEPOSIT".
           DISPLAY "=> PRESS W FOR WITHDRAWAL".
           DISPLAY "=> PRESS T FOR TRANSFER".
           ACCEPT C FROM CONSOLE.
           IF C = 'D' THEN GO TO D.
           IF C = 'W' THEN GO TO W.
           IF C = 'T' THEN GO TO T.
           IF C NOT = 'D' OR NOT ='W' OR NOT = 'T' THEN
               DISPLAY "=> INVALID INPUT", GO TO RE-CHOOSE.

       D.
           DISPLAY "=> AMOUNT".
           ACCEPT AMOUNT FROM CONSOLE.
           MULTIPLY AMOUNT BY 100 GIVING AMOUNT.
           IF AMOUNT <= 0 THEN DISPLAY "=> INVALID INPUT", GO TO D.
           ADD AMOUNT TO WZ-BLC.
           IF ATM_NUM = 1 THEN GO TO WRITE-FILE-711.
           IF ATM_NUM = 2 THEN GO TO WRITE-FILE-713.

       W.
           DISPLAY "=> AMOUNT".
           ACCEPT AMOUNT FROM CONSOLE.
           MULTIPLY AMOUNT BY 100 GIVING AMOUNT.
           IF AMOUNT <= 0 THEN DISPLAY "=> INVALID INPUT", GO TO W.
           IF AMOUNT > WZ-BLC THEN DISPLAY "=> INSUFFICIENT BALANCE",
                                   GO TO W.
           IF ATM_NUM = 1 THEN GO TO WRITE-FILE-711.
           IF ATM_NUM = 2 THEN GO TO WRITE-FILE-713.

       WRITE-FILE-711.
               MOVE ACC TO UPD-NUM.
               MOVE C TO OPER.
               MOVE AMOUNT TO UPD-AMOUNT.
               MOVE TIME-STAMP TO TIME-STAMP-711.
               WRITE UPD-INFO
               END-WRITE.
               ADD 1 TO TIME-STAMP.

           GO TO LAST-PROCEDURE.

       WRITE-FILE-713.
               MOVE ACC TO UPD2-NUM.
               MOVE C TO OPER2.
               MOVE AMOUNT TO UPD2-AMOUNT.
               MOVE TIME-STAMP TO TIME-STAMP-713.
               WRITE UPD2-INFO
               END-WRITE.
               ADD 1 TO TIME-STAMP.
           GO TO LAST-PROCEDURE.


       T.
           DISPLAY "=> TARGET ACCOUNT".
           ACCEPT T-ACC FROM CONSOLE.
           IF T-ACC = ACC THEN
               DISPLAY "=> YOU CANNNOT TRANSFER TO YOURSELF",
               GO TO T.

           GO TO OPEN-MASTER-2.


       OPEN-MASTER-2.
           OPEN INPUT MASTER.
           GO TO READ-FILE-2.

       READ-FILE-2.
           READ MASTER INTO WZ-INFO
               AT END GO TO RE-INPUT-TARGET.
           GO TO CHECK-TARGET.

       CHECK-TARGET.
           IF T-ACC = WZ-NUM THEN GO TO T2.


           IF T-ACC NOT = WZ-NUM THEN GO TO READ-FILE-2.

       RE-INPUT-TARGET.
           CLOSE MASTER.
           DISPLAY "=> TARGET ACCOUNT DOES NOT EXIST".
           DISPLAY "=> TARGET ACCOUNT".
           ACCEPT T-ACC FROM CONSOLE.
           IF T-ACC = ACC THEN
               DISPLAY "=> YOU CANNNOT TRANSFER TO YOURSELF",
               GO TO T.
           GO TO OPEN-MASTER-2.

       T2.
           CLOSE MASTER.
           DISPLAY "=> AMOUNT".
           ACCEPT AMOUNT FROM CONSOLE.
           MULTIPLY AMOUNT BY 100 GIVING AMOUNT.
           IF AMOUNT <= 0 THEN DISPLAY "=> INVALID INPUT", GO TO T3.
           IF AMOUNT > ACCBLC THEN DISPLAY "=> INSUFFICIENT BALANCE.",
                                   GO TO T3.
           IF ATM_NUM = 1 THEN GO TO WRITE-FILE2-711.
           IF ATM_NUM = 2 THEN GO TO WRITE-FILE2-713.

       T3.
           DISPLAY "=> AMOUNT".
           ACCEPT AMOUNT FROM CONSOLE.
           MULTIPLY AMOUNT BY 100 GIVING AMOUNT.
           IF AMOUNT <= 0 THEN DISPLAY "=> INVALID INPUT", GO TO T3.
           IF AMOUNT > ACCBLC THEN DISPLAY "=> INSUFFICIENT BALANCE.",
                                   GO TO T3.

           IF ATM_NUM = 1 THEN GO TO WRITE-FILE2-711.
           IF ATM_NUM = 2 THEN GO TO WRITE-FILE2-713.


       WRITE-FILE2-711.
               MOVE ACC TO UPD-NUM.
               MOVE 'W' TO OPER.
               MOVE AMOUNT TO UPD-AMOUNT.
               MOVE TIME-STAMP TO TIME-STAMP-711.
               WRITE UPD-INFO
               END-WRITE.
               ADD 1 TO TIME-STAMP.

               MOVE T-ACC TO UPD-NUM.
               MOVE 'D' TO OPER.
               MOVE AMOUNT TO UPD-AMOUNT.
               MOVE TIME-STAMP TO TIME-STAMP-711.
               WRITE UPD-INFO
               END-WRITE.
               ADD 1 TO TIME-STAMP.
           GO TO LAST-PROCEDURE.


       WRITE-FILE2-713.
               MOVE ACC TO UPD2-NUM.
               MOVE 'W' TO OPER2.
               MOVE AMOUNT TO UPD2-AMOUNT.
               MOVE TIME-STAMP TO TIME-STAMP-713.
               WRITE UPD2-INFO
               END-WRITE.
               ADD 1 TO TIME-STAMP.

               MOVE T-ACC TO UPD2-NUM.
               MOVE 'D' TO OPER2.
               MOVE AMOUNT TO UPD2-AMOUNT.
               MOVE TIME-STAMP TO TIME-STAMP-713.
               WRITE UPD2-INFO
               END-WRITE.
               ADD 1 TO TIME-STAMP.
           GO TO LAST-PROCEDURE.

       LAST-PROCEDURE.
           DISPLAY "=> CONTINUE?".
           DISPLAY "=> N FOR NO".
           DISPLAY "=> Y FOR YES".
           ACCEPT ASK FROM CONSOLE.
           IF ASK = 'Y' THEN GO TO CHOOSE-ATM.
           IF ASK = 'N' THEN GO TO FINISH.
           IF ASK NOT= 'Y' OR NOT = 'N' THEN DISPLAY "=> INVALID INPUT",
                                               GO TO LAST-PROCEDURE.


       FINISH.
           CLOSE ATM-713.
           CLOSE ATM-711.
           END PROGRAM ATM.
