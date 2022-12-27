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
       PROGRAM-ID. CENTRAL.
       ENVIRONMENT DIVISION.
           INPUT-OUTPUT SECTION.
           FILE-CONTROL.
      *FOR SORT
               SELECT ATM-711-INPUT ASSIGN TO "trans711.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

               SELECT ATM-711-OUTPUT ASSIGN TO "transac_Sorted711.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

               SELECT WORK-711 ASSIGN TO "trans711.txt".

               SELECT ATM-713-INPUT ASSIGN TO "trans713.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

               SELECT ATM-713-OUTPUT ASSIGN TO "transac_Sorted713.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

               SELECT WORK-713 ASSIGN TO "trans713.txt".


               SELECT MERGE-TRANS ASSIGN TO "transac_Sorted.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

               SELECT PRE-MASTER ASSIGN TO "master.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

               SELECT UPD-MASTER ASSIGN TO "master_updated.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

               SELECT NEG-REPORT ASSIGN TO "negReport.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD ATM-711-INPUT.
           01 INFO-711.
               03 NUM-711 PIC 9(16).
               03 OPER-711 PIC A(1).
               03 AMOUNT-711 PIC 9(7).
               03 TIME-STAMP-711 PIC 9(5).

       FD ATM-711-OUTPUT.
           01 SORT-711.
               03 SORT-NUM-711-OUTPUT PIC 9(16).
               03 OPER-711-OUTPUT PIC A(1).
               03 SORT-AMOUNT-711-OUTPUT PIC 9(7).
               03 SORT-TIME-STAMP-711-OUTPUT PIC 9(5).

       SD WORK-711.
           01 WORK-ATM.
               03 WORK-NUM PIC 9(16).
               03 WORK-OPER PIC A(1).
               03 WORK-AMOUNT PIC 9(7).
               03 WORK-TIME-STAMP PIC 9(5).

       FD ATM-713-INPUT.
           01 INFO-713.
               03 NUM-713 PIC 9(16).
               03 OPER-713 PIC A(1).
               03 AMOUNT-713 PIC 9(7).
               03 TIME-STAMP-713 PIC 9(5).

       FD ATM-713-OUTPUT.
           01 SORT-713.
               03 SORT-NUM-713-OUTPUT PIC 9(16).
               03 OPER-713-OUTPUT PIC A(1).
               03 SORT-AMOUNT-713-OUTPUT PIC 9(7).
               03 SORT-TIME-STAMP-713-OUTPUT PIC 9(5).

       SD WORK-713.
           01 WORK-ATM-713.
               03 WORK-NUM-713 PIC 9(16).
               03 WORK-OPER-713 PIC A(1).
               03 WORK-AMOUNT-713 PIC 9(7).
               03 WORK-TIME-STAMP-713 PIC 9(5).



      *merge-sorted file
       FD MERGE-TRANS.
           01 SORT-INFO.
               03 SORT-NUM PIC 9(16).
               03 SORT-OPER PIC A(1).
               03 SORT-AMOUNT PIC S9(7).
               03 SORT-TIME-STAMP PIC 9(5).


      *master file input
       FD PRE-MASTER.
           01 WIZARD-INFO.
               03 ACC-NAME PIC A(20).
               03 ACC-NUM PIC X(16).
               03 ACC-PWD PIC 9(6).
               03 ACC-NEG PIC A(1).
               03 ACC-BLC PIC 9(15).

      *master-upd-output
       FD  UPD-MASTER.
           01 UPD-WIZARD-INFO.
               03 UPD-ACC-NAME PIC A(20).
               03 UPD-ACC-NUM PIC X(16).
               03 UPD-ACC-PWD PIC 9(6).
               03 UPD-ACC-NEG PIC X(1).
               03 UPD-ACC-BLC PIC 9(15).

       FD NEG-REPORT.
           01 NEG-INFO.
               03 HOLDER-NAME PIC X(6).
               03 NEG-NAME PIC A(20).
               03 ACC-NUMBER PIC X(16).
               03 NEG-NUM PIC X(16).
               03 BALANCE PIC X(11).
               03 NEG-BLC PIC 9(15).


       WORKING-STORAGE SECTION.
       01 SORT-711-INFO.
           03 SORT-711-NUM PIC 9(16).
           03 SORT-711-OPER PIC A(1).
           03 SORT-711-AMOUNT PIC 9(7).
           03 SORT-711-TIME-STAMP PIC 9(5).

       01 SORT-713-INFO.
           03 SORT-713-NUM PIC 9(16).
           03 SORT-713-OPER PIC A(1).
           03 SORT-713-AMOUNT PIC 9(7).
           03 SORT-713-TIME-STAMP PIC 9(5).

      *MASTER-FILE-CONSTRUCTURE
       01 WZ-INFO.
           03 WZ-NAME PIC A(20).
           03 WZ-NUM PIC X(16).
           03 WZ-PWD PIC 9(6).
           03 WZ-NEG PIC X(1).
           03 WZ-BLC PIC S9(15).

      *SORTED FILE CONSTRUCTURE.
       01 MERGE-SORT-INFO.
           03 MERGE-SORT-NUM PIC 9(16).
           03 MERGE-SORT-OPER PIC A(1).
           03 MERGE-SORT-AMOUNT PIC S9(7).
           03 MERGE-SORT-TIMESTAMP PIC 9(5).

       01 UPD-MASTER-INFO.
           03 UPD-NAME PIC A(20).
           03 UPD-NUM PIC X(16).
           03 UPD-PWD PIC 9(6).
           03 UPD-NEG PIC X(1).
           03 UPD-BLC PIC 9(15).

       77 FLAG-711 PIC 9(2).
       77 FLAG-713 PIC 9(2).
       77 NEG-FLAG PIC 9(1).

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

      *SORT EACH FILE

           SORT WORK-711 ON ASCENDING KEY SORT-NUM-711-OUTPUT
           ON ASCENDING KEY SORT-TIME-STAMP-711-OUTPUT
           USING ATM-711-INPUT GIVING ATM-711-OUTPUT.

           SORT WORK-713 ON ASCENDING KEY SORT-NUM-713-OUTPUT
           ON ASCENDING KEY SORT-TIME-STAMP-713-OUTPUT
           USING ATM-713-INPUT GIVING ATM-713-OUTPUT.

      *MERGE TWO FILE
           OPEN OUTPUT MERGE-TRANS.
           OPEN INPUT ATM-711-OUTPUT.
           OPEN INPUT ATM-713-OUTPUT.
           MOVE 0 TO FLAG-711.
           MOVE 0 TO FLAG-713.
           GO TO READ-FILE-711.

       READ-FILE-711.
           READ ATM-711-OUTPUT INTO SORT-711-INFO
               AT END GO TO CLOSE-711-FIRST.
           GO TO READ-FILE-713.

       READ-FILE-713.
           READ ATM-713-OUTPUT INTO SORT-713-INFO
               AT END GO TO CLOSE-713-FILE.
           GO TO FLAG-CHECK.

       RE-READ-FILE-711.
           READ ATM-711-OUTPUT INTO SORT-711-INFO
              AT END GO TO CLOSE-711-FILE.
           GO TO FLAG-CHECK.

       CLOSE-711-FIRST.
           ADD 1 TO FLAG-711.
           CLOSE ATM-711-OUTPUT.
           GO TO READ-FILE-713.

       CLOSE-711-FILE.
           ADD 1 TO FLAG-711.
           CLOSE ATM-711-OUTPUT.
           GO TO FLAG-CHECK.

       RE-READ-FILE-713.
           READ ATM-713-OUTPUT INTO SORT-713-INFO
               AT END GO TO CLOSE-713-FILE.
           GO TO FLAG-CHECK.

       CLOSE-713-FILE.
           ADD 1 TO FLAG-713.
           CLOSE ATM-713-OUTPUT.
           GO TO FLAG-CHECK.

       FLAG-CHECK.
           IF FLAG-711 = 0 AND FLAG-713 = 0 THEN
               GO TO NUM-CHECK.

           IF FLAG-711 >= 1 AND FLAG-713 = 0 THEN
               GO TO WRITE-MASTER-713.

           IF FLAG-711 = 0 AND FLAG-713 >= 1 THEN
               GO TO WRITE-MASTER-711.

           IF FLAG-711 = 1 AND FLAG-713 >= 1 THEN
               GO TO FINISH-MERGE.

           IF FLAG-713 = 1 AND FLAG-711 >= 1 THEN
               GO TO FINISH-MERGE.

       NUM-CHECK.
               IF SORT-711-NUM > SORT-713-NUM
                   THEN GO TO WRITE-MASTER-713.

               IF SORT-711-NUM < SORT-713-NUM
                   THEN GO TO WRITE-MASTER-711.

               IF SORT-711-NUM = SORT-713-NUM
                   THEN IF SORT-711-TIME-STAMP > SORT-713-TIME-STAMP
                       THEN GO TO WRITE-MASTER-713.
                        IF SORT-711-TIME-STAMP < SORT-713-TIME-STAMP
                       THEN GO TO WRITE-MASTER-711.


       WRITE-MASTER-711.
           MOVE SORT-711-NUM TO SORT-NUM.
           MOVE SORT-711-OPER TO SORT-OPER.
           MOVE SORT-711-AMOUNT TO SORT-AMOUNT.
           MOVE SORT-711-TIME-STAMP TO SORT-TIME-STAMP.
           WRITE SORT-INFO
           END-WRITE.

           IF FLAG-711 = 0 AND FLAG-713 = 0 THEN
               GO TO RE-READ-FILE-711.

           IF FLAG-711 = 1 AND FLAG-713 = 0 THEN
               ADD 1 TO FLAG-711, GO TO FLAG-CHECK.

           IF FLAG-711 = 0 AND FLAG-713 = 1 THEN
               GO TO RE-READ-FILE-711.

           IF FLAG-711 = 0 AND FLAG-713 > 1 THEN
               GO TO RE-READ-FILE-713.

           IF FLAG-711 = 1 AND FLAG-713 > 1 THEN
               GO TO FINISH-MERGE.

           IF FLAG-713 = 1 AND FLAG-711 > 1 THEN
               GO TO FINISH-MERGE.

       WRITE-MASTER-713.

           MOVE SORT-713-NUM TO SORT-NUM.
           MOVE SORT-713-OPER TO SORT-OPER.
           MOVE SORT-713-AMOUNT TO SORT-AMOUNT.
           MOVE SORT-713-TIME-STAMP TO SORT-TIME-STAMP.
           WRITE SORT-INFO
           END-WRITE.

           IF FLAG-711 = 1 AND FLAG-713 = 0 THEN
               GO TO RE-READ-FILE-713.

           IF FLAG-711 = 0 AND FLAG-713 = 0 THEN
               GO TO RE-READ-FILE-713.

           IF FLAG-711 = 0 AND FLAG-713 = 1 THEN
               ADD 1 TO FLAG-713,
               GO TO FLAG-CHECK.

           IF FLAG-711 > 1 AND FLAG-713 = 0 THEN
               GO TO RE-READ-FILE-713.

           IF FLAG-711 = 1 AND FLAG-713 > 1 THEN
               GO TO FINISH-MERGE.

           IF FLAG-713 = 1 AND FLAG-711 > 1 THEN
               GO TO FINISH-MERGE.

       FINISH-MERGE.

           CLOSE MERGE-TRANS.
           GO TO UPDATE-FILE.


      *UPDATE FILE
       UPDATE-FILE.
           OPEN INPUT MERGE-TRANS.
           OPEN INPUT PRE-MASTER.
           OPEN OUTPUT UPD-MASTER.
           READ MERGE-TRANS INTO MERGE-SORT-INFO.
           READ PRE-MASTER INTO WZ-INFO.
           GO TO CHECK.

       CHECK.
           IF WZ-NUM NOT = MERGE-SORT-NUM THEN
               GO TO NEW-WRITE.

           IF WZ-NUM = MERGE-SORT-NUM THEN
               GO TO CHECK-OPER.

       NEW-WRITE.
           MOVE WZ-NAME TO UPD-ACC-NAME.
           MOVE WZ-NUM TO UPD-ACC-NUM.
           MOVE WZ-PWD TO UPD-ACC-PWD.
           MOVE WZ-NEG TO UPD-ACC-NEG.
           MOVE WZ-BLC TO UPD-ACC-BLC.
           WRITE UPD-WIZARD-INFO
           END-WRITE.

           READ PRE-MASTER INTO WZ-INFO
               AT END GO TO FINISH-UPDATE.
           GO TO CHECK.

       CHECK-OPER.
           IF MERGE-SORT-OPER ="D" THEN
               GO TO ADD-BLC.

           IF MERGE-SORT-OPER ="W" THEN
               GO TO WITH-BLC.

       ADD-BLC.
           IF WZ-NEG = "+" THEN
               ADD MERGE-SORT-AMOUNT TO WZ-BLC.

           IF WZ-NEG = "-" THEN
               MULTIPLY -1 BY WZ-BLC GIVING WZ-BLC,
               ADD MERGE-SORT-AMOUNT TO WZ-BLC.

           IF WZ-BLC > 0 THEN
               MOVE '+' TO WZ-NEG.

           READ MERGE-TRANS INTO MERGE-SORT-INFO
               AT END GO TO NEW-WRITE.

           GO TO CHECK.

       WITH-BLC.
           SUBTRACT MERGE-SORT-AMOUNT FROM WZ-BLC.
           IF WZ-BLC < 0 THEN
               MOVE '-' TO WZ-NEG.
           READ MERGE-TRANS INTO MERGE-SORT-INFO
               AT END GO TO NEW-WRITE.
           GO TO CHECK.



       FINISH-UPDATE.
           CLOSE UPD-MASTER.
           CLOSE PRE-MASTER.
           CLOSE MERGE-TRANS.

      *WRITE NEG-REPORT
       NEG-REPORT-WRITE.
           OPEN INPUT UPD-MASTER.
           OPEN OUTPUT NEG-REPORT.
           GO TO READ-MASTER.

       READ-MASTER.
           READ UPD-MASTER INTO UPD-MASTER-INFO
               AT END GO TO FINISH-REAL.
           GO TO CHECK-NEG.

       CHECK-NEG.

           IF UPD-NEG = '-' THEN
               GO TO NEG-WRITE.
           IF UPD-NEG = '+' THEN
               GO TO READ-MASTER.

       NEG-WRITE.
           MOVE "Name: " TO HOLDER-NAME.
           MOVE UPD-NAME TO NEG-NAME.
           MOVE "Account Number: " TO ACC-NUMBER.
           MOVE UPD-NUM TO NEG-NUM.
           MOVE " Balance: -" TO BALANCE.
           MOVE UPD-BLC TO NEG-BLC.
           WRITE NEG-INFO
           END-WRITE.
           GO TO READ-MASTER.

       FINISH-REAL.
           CLOSE UPD-MASTER.
           CLOSE NEG-REPORT.

       END PROGRAM CENTRAL.
