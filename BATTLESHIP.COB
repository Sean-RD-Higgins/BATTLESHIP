       IDENTIFICATION DIVISION.                                                                                                                   
       PROGRAM-ID. BATTLE.                                                                                                                 
*********DASH HIGGINS PROGRAMMING SEANSUKE@AIM.COM
*********this code is property of sean r higgins failure to give credit will result in instant death by soul stealing
       DATA DIVISION.                                                                                                                             
       WORKING-STORAGE SECTION.                                                                                                                   
*********end of file?  why did i keep this?!!?
       01  EOF             PIC XXX VALUE "NO".                                                                                                              
*********PLAYERS
       01  COORDINATES.
*********the table that actually shows your attacks on the enemy, X or [space] or !
            02  TABLE-WS OCCURS 2 TIMES.                                                                                                                           
                 03  ROWS-WS  OCCURS 9 TIMES.                                                                                                                
                      04  COLUMNS-WS  OCCURS 9 TIMES.
                           05  P   PIC XX.
       01  ITEMS.
*********PLAYER'S ship locations in x and y locations, and the length of the ship
            02  ITEM-TABLE-WS    OCCURS 2 TIMES.                                                                                                                           
                 03  ITEM-ROWS-WS  OCCURS 5 TIMES.                                                                                                                
                      04  P-X      PIC 9 OCCURS 5 TIMES.
                      04  P-Y      PIC 9 OCCURS 5 TIMES.
                      04  P-LENG   PIC 9.
*********player's remaining ship amount
       01  PLAYERS-HP.
            02  P-HP  PIC 9 OCCURS 2 TIMES.
*********DATA
       01  X         PIC 99.
       01  Y         PIC 99.
       01  Z         PIC 99.
*********current player turn
       01  PLAYER    PIC 9.
*********current enemy of current player turn
       01  OPPONENT  PIC 9.
*********check flag
       01  CHK       PIC 9.
*********check location
       01  CHK-X     PIC 9.
       01  CHK-Y     PIC 9.
*********i forgot, something important i think
       01  POS       PIC 99.
*********input to play again
       01  INPUT-YN  PIC X VALUE "Y".
*********input numerical location
       01  INPUT-XY.
             02  INPUT-X   PIC 9 VALUE 0.
             02  INPUT-Y   PIC 9 VALUE 0.
*********looooooooooong message.  too lazy to make an occurs
       01  DISPLAY-WS.
             02  FILLER    PIC XX   VALUE "  ".
             02  INT-10    PIC 9.
             02  FILLER    PIC XX  VALUE "| ".
             02  INT-11    PIC X.
             02  FILLER    PIC XX  VALUE "| ".
             02  INT-12    PIC X.
             02  FILLER    PIC XX  VALUE "| ".
             02  INT-13    PIC X.
             02  FILLER    PIC XX  VALUE "| ".
             02  INT-14    PIC X.
             02  FILLER    PIC XX  VALUE "| ".
             02  INT-15    PIC X.
             02  FILLER    PIC XX  VALUE "| ".
             02  INT-16    PIC X.
             02  FILLER    PIC XX  VALUE "| ".
             02  INT-17    PIC X.
             02  FILLER    PIC XX  VALUE "| ".
             02  INT-18    PIC X.
             02  FILLER    PIC XX  VALUE "| ".
             02  INT-19    PIC X.
             02  FILLER    PIC X(6)  VALUE "| ".
             02  FILLER    PIC XX  VALUE "| ".
             02  INT-21    PIC X.
             02  FILLER    PIC XX  VALUE "| ".
             02  INT-22    PIC X.
             02  FILLER    PIC XX  VALUE "| ".
             02  INT-23    PIC X.
             02  FILLER    PIC XX  VALUE "| ".
             02  INT-24    PIC X.
             02  FILLER    PIC XX  VALUE "| ".
             02  INT-25    PIC X.
             02  FILLER    PIC XX  VALUE "| ".
             02  INT-26    PIC X.
             02  FILLER    PIC XX  VALUE "| ".
             02  INT-27    PIC X.
             02  FILLER    PIC XX  VALUE "| ".
             02  INT-28    PIC X.
             02  FILLER    PIC XX  VALUE "| ".
             02  INT-29    PIC X.
             02  FILLER    PIC XX  VALUE "| ".
             02  INT-20    PIC 9.
*********best GUI ever
       01 ASK-LINE.
             02  FILLER   PIC X(8)  VALUE "PLAYER ".
             02  P-CUR    PIC 9.
             02  FILLER   PIC X(5)   VALUE "  HP:".
             02  P-HP-OUT PIC 9.
             02  FILLER   PIC X(50)  VALUE "/5  CHOOSE A LOCATION:  E.G. 39 GIVES X=30 Y=9>".       
*********let's play ball
       PROCEDURE DIVISION.                                                                                                                         
*********omg let's play battleship
                       DISPLAY "BATTLESHIP".
                       PERFORM GAME-FNC.
*********ok we're done
                       DISPLAY "GAME OVER!".                                                          
                       STOP RUN.
       INIT-ITEMS.
****************PLAYER1
               MOVE 1 TO PLAYER.
               PERFORM INIT-PLAYER.
****************PLAYER2
               MOVE 2 TO PLAYER.
               PERFORM INIT-PLAYER.
****************DONE WITH SHIP INIT
               MOVE 1 TO P-CUR.
       INIT-PLAYER.
               MOVE SPACES TO TABLE-WS(PLAYER).
               MOVE 0 TO ITEM-TABLE-WS(PLAYER).
               MOVE 1 TO P-HP(PLAYER).
****************2 UNIT SHIP
               MOVE 2 TO P-LENG(PLAYER, 1).
               COMPUTE X = 1 + FUNCTION RANDOM * 7.                                                                                         
               COMPUTE Y = 1 + FUNCTION RANDOM * 8.                                                                                        
               COMPUTE P-X(PLAYER, 1, 1) = X.
               COMPUTE P-Y(PLAYER, 1, 1) = Y.
               COMPUTE P-X(PLAYER, 1, 2) = X + 1.
               COMPUTE P-Y(PLAYER, 1, 2) = Y.
               ADD 1 TO P-HP(PLAYER).
****************3 UNIT SHIP, HORIZONTAL
               MOVE 3 TO P-LENG(PLAYER, 2).
               MOVE 0 TO CHK.
               PERFORM UNTIL CHK = 1
                     MOVE 1 TO CHK
                     COMPUTE X = 1 + FUNCTION RANDOM * 6                                                                                         
                     COMPUTE Y = 1 + FUNCTION RANDOM * 8
                           PERFORM VARYING CHK-Y FROM 1 BY 1 UNTIL CHK-Y > 2 OR CHK = 2                                        
                                 IF P-X(PLAYER, 1, CHK-Y) = X AND P-Y(PLAYER, 1, CHK-Y) = Y THEN
                                       MOVE 2 TO CHK
                                 END-IF
                                 IF P-X(PLAYER, 1, CHK-Y) = X + 1 AND P-Y(PLAYER, 1, CHK-Y) = Y THEN
                                       MOVE 2 TO CHK
                                 END-IF
                                 IF P-X(PLAYER, 1, CHK-Y) = X + 2 AND P-Y(PLAYER, 1, CHK-Y) = Y THEN
                                       MOVE 2 TO CHK
                                 END-IF
                           END-PERFORM
               END-PERFORM.
               COMPUTE P-X(PLAYER, P-HP(PLAYER), 1) = X.                                                                                         
               COMPUTE P-Y(PLAYER, P-HP(PLAYER), 1) = Y.                                                                                        
               COMPUTE P-X(PLAYER, P-HP(PLAYER), 2) = X + 1.                                                                                         
               COMPUTE P-Y(PLAYER, P-HP(PLAYER), 2) = Y.                                                                                        
               COMPUTE P-X(PLAYER, P-HP(PLAYER), 3) = X + 2.                                                                                         
               COMPUTE P-Y(PLAYER, P-HP(PLAYER), 3) = Y.                                                                                        
               ADD 1 TO P-HP(PLAYER).
****************3 UNIT SHIP, VERTICAL
               MOVE 3 TO P-LENG(PLAYER, 3).
               MOVE 0 TO CHK.
               PERFORM UNTIL CHK = 1
                     MOVE 1 TO CHK
                     COMPUTE X = 1 + FUNCTION RANDOM * 8                                                                                         
                     COMPUTE Y = 1 + FUNCTION RANDOM * 6
                     PERFORM VARYING CHK-X FROM 1 BY 1 UNTIL CHK-X > 2 OR CHK = 2
                           PERFORM VARYING CHK-Y FROM 1 BY 1 UNTIL CHK-Y > 3 OR CHK = 2                                        
                                 IF P-X(PLAYER, CHK-X, CHK-Y) = X AND P-Y(PLAYER, CHK-X, CHK-Y) = Y THEN
                                       MOVE 2 TO CHK
                                 END-IF
                                 IF P-X(PLAYER, CHK-X, CHK-Y) = X AND P-Y(PLAYER, CHK-X, CHK-Y) = Y + 1 THEN
                                       MOVE 2 TO CHK
                                 END-IF
                                 IF P-X(PLAYER, CHK-X, CHK-Y) = X AND P-Y(PLAYER, CHK-X, CHK-Y) = Y + 2 THEN
                                       MOVE 2 TO CHK
                                 END-IF
                           END-PERFORM
                     END-PERFORM
               END-PERFORM.
               COMPUTE P-X(PLAYER, P-HP(PLAYER), 1) = X.                                                                                         
               COMPUTE P-Y(PLAYER, P-HP(PLAYER), 1) = Y.                                                                                        
               COMPUTE P-X(PLAYER, P-HP(PLAYER), 2) = X.                                                                                         
               COMPUTE P-Y(PLAYER, P-HP(PLAYER), 2) = Y + 1.                                                                                        
               COMPUTE P-X(PLAYER, P-HP(PLAYER), 3) = X.                                                                                         
               COMPUTE P-Y(PLAYER, P-HP(PLAYER), 3) = Y + 2.                                                                                        
               ADD 1 TO P-HP(PLAYER).
****************4 UNIT SHIP, HORIZONTAL
               MOVE 4 TO P-LENG(PLAYER, 4).
               MOVE 0 TO CHK.
               PERFORM UNTIL CHK = 1
                     MOVE 1 TO CHK
                     COMPUTE X = 1 + FUNCTION RANDOM * 5                                                                                         
                     COMPUTE Y = 1 + FUNCTION RANDOM * 8
                     PERFORM VARYING CHK-X FROM 1 BY 1 UNTIL CHK-X > 3 OR CHK = 2
                           PERFORM VARYING CHK-Y FROM 1 BY 1 UNTIL CHK-Y > 3 OR CHK = 2
                                 IF P-X(PLAYER, CHK-X, CHK-Y) = X AND P-Y(PLAYER, CHK-X, CHK-Y) = Y THEN
                                       MOVE 2 TO CHK
                                 END-IF
                                 IF P-X(PLAYER, CHK-X, CHK-Y) = X + 1 AND P-Y(PLAYER, CHK-X, CHK-Y) = Y THEN
                                       MOVE 2 TO CHK
                                 END-IF
                                 IF P-X(PLAYER, CHK-X, CHK-Y) = X + 2 AND P-Y(PLAYER, CHK-X, CHK-Y) = Y THEN
                                       MOVE 2 TO CHK
                                 END-IF
                                 IF P-X(PLAYER, CHK-X, CHK-Y) = X + 3 AND P-Y(PLAYER, CHK-X, CHK-Y) = Y THEN
                                       MOVE 2 TO CHK
                                 END-IF
                           END-PERFORM
                     END-PERFORM
               END-PERFORM.
               COMPUTE P-X(PLAYER, P-HP(PLAYER), 1) = X.                                                                                         
               COMPUTE P-Y(PLAYER, P-HP(PLAYER), 1) = Y.                                                                                        
               COMPUTE P-X(PLAYER, P-HP(PLAYER), 2) = X + 1.                                                                                         
               COMPUTE P-Y(PLAYER, P-HP(PLAYER), 2) = Y.                                                                                        
               COMPUTE P-X(PLAYER, P-HP(PLAYER), 3) = X + 2.                                                                                         
               COMPUTE P-Y(PLAYER, P-HP(PLAYER), 3) = Y.                                                                                        
               COMPUTE P-X(PLAYER, P-HP(PLAYER), 4) = X + 3.                                                                                         
               COMPUTE P-Y(PLAYER, P-HP(PLAYER), 4) = Y.                                                                                        
               ADD 1 TO P-HP(PLAYER).
****************5 UNIT SHIP, VERTICAL
               MOVE 5 TO P-LENG(PLAYER, 5).
               MOVE 0 TO CHK.
               PERFORM UNTIL CHK = 1
                     MOVE 1 TO CHK
                     COMPUTE X = 1 + FUNCTION RANDOM * 8                                                                                         
                     COMPUTE Y = 1 + FUNCTION RANDOM * 4
                     PERFORM VARYING CHK-X FROM 1 BY 1 UNTIL CHK-X > 4 OR CHK = 2
                           PERFORM VARYING CHK-Y FROM 1 BY 1 UNTIL CHK-Y > 4 OR CHK = 2                                        
                                 IF P-X(PLAYER, CHK-X, CHK-Y) = X AND P-Y(PLAYER, CHK-X, CHK-Y) = Y THEN
                                       MOVE 2 TO CHK
                                 END-IF
                                 IF P-X(PLAYER, CHK-X, CHK-Y) = X AND P-Y(PLAYER, CHK-X, CHK-Y) = Y + 1 THEN
                                       MOVE 2 TO CHK
                                 END-IF
                                 IF P-X(PLAYER, CHK-X, CHK-Y) = X AND P-Y(PLAYER, CHK-X, CHK-Y) = Y + 2 THEN
                                       MOVE 2 TO CHK
                                 END-IF
                                 IF P-X(PLAYER, CHK-X, CHK-Y) = X AND P-Y(PLAYER, CHK-X, CHK-Y) = Y + 3 THEN
                                       MOVE 2 TO CHK
                                 END-IF
                                 IF P-X(PLAYER, CHK-X, CHK-Y) = X AND P-Y(PLAYER, CHK-X, CHK-Y) = Y + 4 THEN
                                       MOVE 2 TO CHK
                                 END-IF
                           END-PERFORM
                     END-PERFORM
               END-PERFORM.
               COMPUTE P-X(PLAYER, P-HP(PLAYER), 1) = X.                                                                                         
               COMPUTE P-Y(PLAYER, P-HP(PLAYER), 1) = Y.                                                                                        
               COMPUTE P-X(PLAYER, P-HP(PLAYER), 2) = X.                                                                                         
               COMPUTE P-Y(PLAYER, P-HP(PLAYER), 2) = Y + 1.                                                                                        
               COMPUTE P-X(PLAYER, P-HP(PLAYER), 3) = X.                                                                                         
               COMPUTE P-Y(PLAYER, P-HP(PLAYER), 3) = Y + 2.                                                                                        
               COMPUTE P-X(PLAYER, P-HP(PLAYER), 4) = X.                                                                                         
               COMPUTE P-Y(PLAYER, P-HP(PLAYER), 4) = Y + 3.                                                                                        
               COMPUTE P-X(PLAYER, P-HP(PLAYER), 5) = X.                                                                                         
               COMPUTE P-Y(PLAYER, P-HP(PLAYER), 5) = Y + 4.                                                                                        
       GAME-FNC.
***************lay til u cant play no more
               PERFORM UNTIL INPUT-YN = "N"
*********************init everything
                     PERFORM INIT-ITEMS
                     PERFORM UNTIL P-HP(1) = 0 OR P-HP(2) = 0
********************************PLAYER 1 TURN
                             MOVE 1 TO PLAYER
                             MOVE 2 TO OPPONENT
                             PERFORM DISPLAY-FNC
                             PERFORM INPUT-FNC
                             PERFORM ACTION-FNC
********************************PLAYER 2 TURN
                             MOVE 2 TO PLAYER
                             MOVE 1 TO OPPONENT
                             PERFORM DISPLAY-FNC
                             PERFORM INPUT-FNC
                             PERFORM ACTION-FNC
                     END-PERFORM
************************cobol doesnt have spell-check
                     IF P-HP(1) = 0 THEN
                             DISPLAY "PLAYER 2 WINS!"
                             DISPLAY "YOU WIN EPICALLY!"
                     ELSE 
                             IF P-HP(2) = 0 THEN
                                    DISPLAY "PLAYER 1 WINS!"
                                    DISPLAY "YOU WIN EPICALLY!"
                             ELSE
                                    DISPLAY "DRAW"
                             END-IF
                     END-IF
************************play again?
                     PERFORM INPUT-YN-FNC
               END-PERFORM.
       INPUT-YN-FNC.
               PERFORM UNTIL INPUT-YN = "Y" OR INPUT-YN = "N"
                       DISPLAY "PLAY AGAIN? Y/N >"
                       ACCEPT INPUT-YN FROM CONSOLE
               END-PERFORM.
       DISPLAY-FNC.
               DISPLAY "Y X|10|20|30|40|50|60|70|80|90|     |10|20|30|40|50|60|70|80|90|X Y".
               PERFORM VARYING Y FROM 1 BY 1 UNTIL Y > 9
                       DISPLAY "---+--+--+--+--+--+--+--+--+--|     |---+--+--+--+--+--+--+--+--+--"
                       PERFORM DISPLAY2-FNC
               END-PERFORM.
               DISPLAY "-------------------------------     -------------------------------".
       DISPLAY2-FNC.
************************i love making confusing display code
               MOVE Y TO INT-10.
               MOVE P(1, 1, Y) TO INT-11.
               MOVE P(1, 2, Y) TO INT-12.
               MOVE P(1, 3, Y) TO INT-13.
               MOVE P(1, 4, Y) TO INT-14.
               MOVE P(1, 5, Y) TO INT-15.
               MOVE P(1, 6, Y) TO INT-16.
               MOVE P(1, 7, Y) TO INT-17.
               MOVE P(1, 8, Y) TO INT-18.
               MOVE P(1, 9, Y) TO INT-19.
               MOVE P(2, 1, Y) TO INT-21.
               MOVE P(2, 2, Y) TO INT-22.
               MOVE P(2, 3, Y) TO INT-23.
               MOVE P(2, 4, Y) TO INT-24.
               MOVE P(2, 5, Y) TO INT-25.
               MOVE P(2, 6, Y) TO INT-26.
               MOVE P(2, 7, Y) TO INT-27.
               MOVE P(2, 8, Y) TO INT-28.
               MOVE P(2, 9, Y) TO INT-29.
               MOVE Y TO INT-20.
               DISPLAY DISPLAY-WS.
       INPUT-FNC.
               MOVE PLAYER TO P-CUR.
               MOVE 0 TO INPUT-X.
               MOVE 0 TO INPUT-Y.
               PERFORM UNTIL INPUT-XY >= 11 AND INPUT-XY <= 99
                       MOVE P-HP(PLAYER) TO P-HP-OUT
                       DISPLAY ASK-LINE
                       ACCEPT INPUT-XY FROM CONSOLE
****************************for people who can't follow basic instructions
                       IF INPUT-XY < 11 OR INPUT-XY > 99 THEN
                             DISPLAY "MUST BE A NUMBER BETWEEN 11 AND 99"
                       END-IF
****************************i should just let you waste your turn hitting a location you already tried
                       IF P(PLAYER, INPUT-X, INPUT-Y) NOT = " " THEN
                             DISPLAY "YOU HAVE ALREADY TRIED THAT LOCATION!"
                             MOVE 0 TO INPUT-XY
                       END-IF
               END-PERFORM.
****************************i forgot why i bothered duplicating data
               MOVE INPUT-X TO X.
               MOVE INPUT-Y TO Y.
       ACTION-FNC.
****************************i dare you to understand this
               MOVE 0 TO CHK.
                     PERFORM VARYING CHK-X FROM 1 BY 1 UNTIL CHK-X > 5
                           PERFORM VARYING CHK-Y FROM 1 BY 1 UNTIL CHK-Y > 5                                        
                                 IF P-X(OPPONENT, CHK-X, CHK-Y) = X AND P-Y(OPPONENT, CHK-X, CHK-Y) = Y THEN
                                       DISPLAY "HIT"
                                       COMPUTE P-LENG(OPPONENT, CHK-X) = P-LENG(OPPONENT, CHK-X) - 1
                                       IF P-LENG(OPPONENT, CHK-X) = 0 THEN
                                             DISPLAY "YOU SUNK MY BATTLESHIP!"
                                             COMPUTE P-HP(OPPONENT) = P-HP(OPPONENT) - 1
                                       END-IF
                                       MOVE 2 TO CHK
                                 END-IF
                           END-PERFORM
                     END-PERFORM
****************************prepare to miss... ALOT
               IF CHK = 0 THEN
                       MOVE "X" TO P(PLAYER, X, Y)
                       DISPLAY "MISS"
               ELSE 
****************************can you swim, nelson?
                       MOVE "!" TO P(PLAYER, X, Y)
               END-IF.
****************************never gonna give you up
****************************Never gonna let you down,
****************************Never gonna run around and desert you.
****************************Never gonna make you cry,
****************************Never gonna say goodbye,
****************************Never gonna tell a lie and hurt you.
****************************you have just been rick rolled
