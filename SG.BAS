' A few touch-ups to make then seperate into
' 2 files then EXE them and link them....
 DIM WinMining AS DOUBLE, WinVacation AS DOUBLE, WinShipYards AS DOUBLE
 DIM WinAgriculture AS DOUBLE

 DECLARE SUB Covert ()
 DECLARE SUB Status ()
 DECLARE SUB BuyOrSell ()
 DECLARE SUB Sell ()
 DECLARE SUB Battle ()
 DECLARE SUB GameOver ()
 DECLARE SUB ReadEmail ()
 DECLARE SUB EmailSend (text$)
 DECLARE SUB WriteEmail ()
 DECLARE SUB Score ()
 DECLARE SUB Bank ()
 DECLARE SUB Slots ()
 DECLARE SUB Hints ()
 DECLARE SUB Talking ()
 DECLARE SUB Move (X, Y)
 DECLARE SUB EndTurn ()
 DECLARE SUB Bar ()
 DECLARE SUB Buy ()
 DECLARE SUB Main ()
 DECLARE SUB UpDate ()
 DECLARE SUB KeyPress (text$)
 DECLARE SUB Pause ()
 DECLARE SUB News ()
 DECLARE SUB Game ()
 DECLARE SUB Start ()
 DECLARE SUB Chat ()
 DECLARE SUB ReSetFiles ()
 DECLARE SUB q (text$)
 DECLARE FUNCTION ran (Low%, High%)
  TYPE Record
     UserName AS STRING * 30
     EmpireName AS STRING * 30
     CommanderName AS STRING * 30
     CommanderSex AS STRING * 1
     LastOn AS STRING * 10
     TurnsLeft AS DOUBLE
     Population AS DOUBLE
     Food AS DOUBLE
     Money AS DOUBLE
     StarShips AS DOUBLE
     Troops AS DOUBLE
     Stations AS DOUBLE
     Mining AS DOUBLE
     Vacation AS DOUBLE
     Agriculture AS DOUBLE
     ShipYards AS DOUBLE
     Nuclear AS DOUBLE
     Missiles AS DOUBLE
     Spies AS DOUBLE
     Protection AS DOUBLE
     Letter AS STRING * 1
     NetWorth AS DOUBLE
     Special AS STRING * 1
     MiningPrice AS DOUBLE
     VacationPrice AS DOUBLE
     AgriculturePrice AS DOUBLE
     ShipYardsPrice AS DOUBLE
     InBank AS DOUBLE
  END TYPE
  TYPE Conversation
     CommanderName AS STRING * 30
     Talking AS STRING * 255
  END TYPE
  TYPE Action
     EmpireName AS STRING * 30
     Action AS STRING * 500
  END TYPE
  TYPE msgs
     EmpireLetter AS STRING * 26
     SentTo AS STRING * 26
     SenderName AS STRING * 30
     Subject AS STRING * 75
     DateSent AS STRING * 10
     Message1 AS STRING * 76
     Message2 AS STRING * 76
     Message3 AS STRING * 76
     Message4 AS STRING * 76
     Message5 AS STRING * 76
     Message6 AS STRING * 76
     Message7 AS STRING * 76
     Message8 AS STRING * 76
     Message9 AS STRING * 76
     Message10 AS STRING * 76
     Message11 AS STRING * 76
     Message12 AS STRING * 76
     Message13 AS STRING * 76
     Message14 AS STRING * 76
     Message15 AS STRING * 76
     Message16 AS STRING * 76
     Message17 AS STRING * 76
     Message18 AS STRING * 76
     Message19 AS STRING * 76
     Message20 AS STRING * 76
  END TYPE
  TYPE Nuc
     EmpireName AS STRING * 30
     Ammount AS DOUBLE
     Attacker AS STRING * 30
  END TYPE
  COMMON SHARED Player AS Record, Attack AS Record, Uper AS Record
  COMMON SHARED Talk AS Conversation
  COMMON SHARED Event AS Action
  COMMON SHARED Messages AS msgs, Msg AS msgs
  COMMON SHARED Nuke AS Nuc
  COMMON SHARED SayA$, SayB$, SayC$, SayD$, SayE$, SayF$, SayG$, SayH$, SayI$, SayJ$
  COMMON SHARED SayK$, SayL$, SayM$, SayN$, SayO$, SayP$, SayQ$, SayR$, SayS$, SayT$
  COMMON SHARED SayU$, SayV$, SayW$, SayX$, SayY$, SayZ$, Say1$, Say2$, Say3$, Say4$
  COMMON SHARED Say5$, Say6$, Say7$, Say8$, Say9$, Say0$
  COMMON SHARED CivilWar, BattlesLeft

' $INCLUDE: 'DOORSORC.INT'


' *** Pre-Program Junk ***
  RANDOMIZE TIMER
  PROGNAME$ = "SpaceGuard"
  RELEASE$ = "1.00"
  ClrScrn
  CLOSE #1
  Exit.Dor.1$ = "@X09Exiting SpaceGuard...@X0B"
  Exit.Dor.2$ = "@X09Now Returning to @X0F@BOARDNAME@........."
  Page.String$ = "@X09Paging " + SysFirst$ + "@X0F..........."
  Not.Around$ = "@X09" + SysFirst$ + " is not around now!"


' *** Function Keys ***
  KEY(10) ON
  ON KEY(10) GOSUB chatter
  GOTO skipchat
er:
  ClrScrn
  NL 1
  q "@X04There has been a system error!  Please notify the SysOp!"
  q "Error Code:" + STR$(ERR) + "    Exiting to BBS.... now."
  ExitDoor

chatter:
  Chat
  RETURN

skipchat:

' *** Reset Maybe? ***
  IF INSTR(UCASE$(COMMAND$), "RESET") THEN
     ReSetFiles
  END IF
' *** Record Handling ***
  OPEN "sggame.dat" FOR INPUT AS #1
  INPUT #1, LettersUsed$
  CLOSE #1



' *** Program ***
  IF ANSI = 0 THEN
     QSend "Sorry @FIRST@, this is an ANSI ONLY door.", Yes
     ExitDoor
  END IF
  ClrScrn
  Move 5, 10
  q "@X0BWelcome!  You are about to enter the first no-registration door!"
  q "@X0B     This  door is currently in BETA testing,  and there  are bound to be"
  q "@X0B     bugs.   I have set  up an  error  trapping routine that  SHOULD stop"
  q "@X0B     all errors,  but occasionaly,  you  may get a warning and be  bumped"
  q "@X0B     back to @BOARDNAME@.  Remember, BETA TESTING...."
  Pause
  ClrScrn
  ViewFile "sg" + MID$(STR$(INT(ran(1, 3))), 2, 2) + ".ans", No, No, No
  Pause
  ClrScrn
  recpos = 3
  OPEN "sgtalk.dat" FOR RANDOM AS #4 LEN = LEN(Talk)
  OPEN "sggame2.dat" FOR RANDOM AS #2 LEN = LEN(Player)
  OPEN "sgevent.dat" FOR RANDOM AS #5 LEN = LEN(Event)
  OPEN "sgmsgs.dat" FOR RANDOM AS #6 LEN = LEN(Messages)
  OPEN "sgnuke.dat" FOR RANDOM AS #7 LEN = LEN(Nuke)
  Max = LOF(2) / LEN(Player)
  FOR i = 1 TO Max
     GET #2, i, Player
     IF RTRIM$(Player.UserName) = CallName$ THEN
          FOR n = 1 TO LOF(7) / LEN(Nuke)
               GET #7, n, Nuke
               IF Nuke.EmpireName = Player.EmpireName THEN
                    WinMining = INT(Nuke.Ammount * 20)
                    WinVacation = INT(Nuke.Ammount * 20)
                    WinShipYards = INT(Nuke.Ammount * 20)
                    WinAgriculture = INT(Nuke.Ammount * 20)
                    Player.Mining = Player.Mining - WinMining
                    Player.Vacation = Player.Vacation - WinVacation
                    Player.ShipYards = Player.ShipYards - WinShipYards
                    Player.Agriculture = Player.Agriculture - WinAgriculture
                    IF Player.Mining < 0 THEN Player.Mining = 0
                    IF Player.Vacation < 0 THEN Player.Vacation = 0
                    IF Player.ShipYards < 0 THEN Player.ShipYards = 0
                    IF Player.Agriculture < 0 THEN Player.Agriculture = 0
                    IF Player.Mining + Player.Vacation + Player.ShipYards + Player.Agriculture = 0 THEN
                         Player.LastOn = "**********"
                    END IF
                    q "@X04You were hit by" + RTRIM$(STR$(Nuke.Ammount)) + " Nukes!"
                    UpDate
                    FOR evn = 1 TO LOF(5) / LEN(Event)
                         GET #5, evn, Event
                         IF Event.EmpireName = "******************************" THEN
                              Event.EmpireName = Nuke.Attacker
                              Event.Action = "@X0BMessage From Nuclear Track Radar: 'Contact!  Target Hit!'"
                              PUT #5, evn, Event
                              GOTO AllDone4
                         END IF
                    NEXT evn
                    Event.EmpireName = Nuke.Attacker
                    Event.Action = "@X0BMessage From Nuclear Track Radar: 'Contact!  Target Hit!'"
                    PUT #5, LOF(5) / LEN(Event) + 1, Event
AllDone4:
               END IF
          NEXT n
          IF Player.LastOn = "**********" THEN
               q "@X0B You're Dead!  Play tomarrow!"
               ExitDoor
          ELSEIF Player.LastOn <> DATE$ THEN
               Player.TurnsLeft = 20
               Player.LastOn = DATE$
          END IF
          Start
     END IF
  NEXT i
NewEmpire:
  IF LEN(LettersUsed$) >= 26 THEN
  ClrScrn
  QSend "@X09All open slot are used!", Yes
  ExitDoor
  END IF
  ClrScrn
  Table$ = "YNyn"
  QSend "@X09A group of collonists has nominated you for the position of Commander!", Yes
  No.Enter.Send = Yes
  SayY$ = "@X0BYes"
  SayN$ = "@X0BNo"
  KeyPress "@X09Do you accept (@X0BYes@X09/@X0BNo@X09)"
  Table$ = Default.Table$
  IF UCASE$(ARG$) = "Y" THEN
     ARG$ = "Y"
  ELSEIF UCASE$(ARG$) = CHR$(13) THEN
     QSend "@X0BYes", Yes
     ARG$ = "Y"
  ELSEIF UCASE$(ARG$) = "N" THEN
     ARG$ = "N"
  END IF
  IF UCASE$(ARG$) = "N" THEN
  QSend "", Yes
  QSend "@X0F'Very well.  We can't have a commander that is not sure of himself.'", Yes
  QSend "@X0BYou hitch a ride of the planet on a transport.  But that regret for", Yes
  QSend "@X0Bturning down such an offer risides in your soul.....", Yes
  ExitDoor
  END IF

RechooseEmpire:
  FOR i = 1 TO Max
     GET #2, i, Attack
     IF RTRIM$(Attack.UserName) = "******************************" THEN
          newloc = i
          GOTO newlocker
     END IF
  NEXT i
  newloc = LOF(2) \ LEN(Player) + 1
newlocker:
  Incomm "@X09Enter your empire's name@X0B: ", No, 30, 9
  Max = LOF(2) / LEN(Attack)
  FOR i = 1 TO Max
     GET #2, i, Attack
     IF RTRIM$(Attack.EmpireName) = ARG$ THEN
          q "@X0FName already being used!"
          GOTO RechooseEmpire
     END IF
  NEXT i
  NewEmpireName$ = ARG$
  Table$ = "YyNn*"
  SayY$ = "@X0FYes"
  SayN$ = "@X0FNo"
  KeyPress "@X09Name you empire @X0F" + NewEmpireName$ + "@X09? (@X0BY@X09/@X0Bn@X09) "
  Table$ = Default.Table$
  IF ARG$ = "*" THEN
     QSend "@X0FSpecial", Yes
     Protected.Input$ = "*"
     Incomm "@X0CPassword 1: @X04", No, 0, f
     Password1$ = ARG$
     Incomm "@X0CPassword 2: @X04", No, 0, f
     Password2$ = ARG$
     Incomm "@X0CPassword 3: @X04", No, 0, f
     Password3$ = ARG$
     Protected.Input$ = ""
     IF Password1$ = "1234" AND Password2$ = "1234" AND Password3$ = "1234" THEN
          RainbowSend "Correct.  Welcome aboard!", No, Yes, 9
          Special = Yes
     ELSE
          GOTO NewEmpire
     END IF
  ELSEIF UCASE$(ARG$) = "Y" THEN
  GOSUB MakeNewEmpire
  ELSEIF UCASE$(ARG$) = "N" THEN
  GOTO NewEmpire
  END IF
MakeNewEmpire:
  Table$ = "ABCDEFGHIJKLMNOPQRSTUVQWXYZabcdefghijklmnopqrstuvwxwyz"
  Incomm "@X09Enter the letter you wish to be (@X0BA-Z@X09):", No, -1, 11
  ARG$ = UCASE$(ARG$)
  IF INSTR(LettersUsed$, ARG$) THEN
     QSend "@X0FThat slot is being used!", Yes
     GOTO MakeNewEmpire
  END IF
  Letter$ = ARG$
  Table$ = Default.Table$
  OPEN "sggame.dat" FOR OUTPUT AS #3
  PRINT #3, LettersUsed$ + ARG$
  CLOSE #3
RechooseCommander:
  QSend "@X09Your people need to know what to call you.", Yes
  User.Color = 15
  Incomm "@X09Enter your allias: ", No, 30, 9
  Max = LOF(2) / LEN(Attack)
  FOR i = 1 TO Max
     GET #2, i, Attack
     IF RTRIM$(Attack.CommanderName) = ARG$ THEN
          q "@X0FName already being used!"
          GOTO RechooseCommander
     END IF
  NEXT i
  NewCommanderName$ = ARG$
  Table$ = "MFmf"
  SayM$ = "@X0BMale"
  SayF$ = "@X0BFemale"
  KeyPress "@X09Are you [@X0BM@X09]@X0Fale@X09/[@X0BF@X09]@X0Female?"
  Table$ = Default.Table$
  ARG$ = UCASE$(ARG$)
  IF ARG$ = "M" THEN
     NewCommanderSex$ = "M"
  ELSEIF ARG$ = CHR$(13) THEN
     QSend "@X0BMale", Yes
     NewCommanderSex$ = "M"
  ELSEIF ARG$ = "N" THEN
     NewCommanderSex$ = "F"
  END IF
  Table$ = "YyNn"
  SayY$ = "@X0BYes"
  SayN$ = "@X0BNo"
  KeyPress "@X09Is ALL this info correct?"
  Table$ = Default.Table$
  IF ARG$ = "N" THEN
     GOTO NewEmpire
  END IF
  User.Color = 15
  IF Special = Yes THEN
     Player.UserName = CallName$
     Player.EmpireName = NewEmpireName$
     Player.CommanderName = NewCommanderName$
     Player.CommanderSex = NewCommanderSex$
     Player.LastOn = DATE$
     Player.TurnsLeft = 20
     Player.Population = 2000000
     Player.Food = 50000000
     Player.Money = 50000000
     Player.StarShips = 75000
     Player.Troops = 250000
     Player.Stations = 125000
     Player.Mining = 350
     Player.Vacation = 450
     Player.Agriculture = 300
     Player.ShipYards = 400
     Player.Nuclear = 200
     Player.Missiles = 1000
     Player.Spies = 1000
     Player.Protection = 0
     Player.Letter = Letter$
     Player.NetWorth = 10000
     Player.Special = "Y"
     Player.MiningPrice = 100
     Player.VacationPrice = 125
     Player.AgriculturePrice = 85
     Player.ShipYardsPrice = 90
     Player.InBank = 500000000
  ELSE
     Player.UserName = CallName$
     Player.EmpireName = NewEmpireName$
     Player.CommanderName = NewCommanderName$
     Player.CommanderSex = NewCommanderSex$
     Player.LastOn = DATE$
     Player.TurnsLeft = 20
     Player.Population = 300
     Player.Food = 10000
     Player.Money = 0
     Player.StarShips = 0
     Player.Troops = 300
     Player.Stations = 0
     Player.Mining = 5
     Player.Vacation = 0
     Player.Agriculture = 2
     Player.ShipYards = 0
     Player.Nuclear = 0
     Player.Missiles = 0
     Player.Spies = 0
     Player.Protection = 20
     Player.Letter = Letter$
     Player.NetWorth = 10000
     Player.Special = "N"
     Player.MiningPrice = 100
     Player.VacationPrice = 125
     Player.AgriculturePrice = 85
     Player.ShipYardsPrice = 90
     Player.InBank = 0
  END IF
  OPEN "SgToday.Ans" FOR APPEND AS #8
  PRINT #8, "[255D[1C[0;36m------ [1;37mA New Empire is Born!  " + RTRIM$(Player.EmpireName) + " is commanded by"
  PRINT #8, "[8C" + RTRIM$(Player.CommanderName) + ".  Earth has high hopes for " + RTRIM$(Player.EmpireName) + "! [0;36m------"
  CLOSE #8
  PUT #2, newloc, Player
  Start

SUB Bank
  ClrScrn
  NL 1
  q "@X0B         Universal Bank"
  q "[1;34m------------------------------------------------------------"
  q "[7C[[37m1[34m]  [37mDeposit Money"
  q "[34m[7C[[37m2[34m]  [37mWithdraw Money"
  q "[34m[7C[[37m3[34m]  [37mTransfer Money"
  q "[1;34m------------------------------------------------------------"
  q "@X03You have" + STR$(Player.InBank) + " in the bank and" + STR$(Player.Money) + " on hand."
  Table$ = "123Qq"
  Say1$ = "@X0BDeposit Money"
  Say2$ = "@X0BWithdraw Money"
  Say3$ = "@X0BTransfer Money"
  SayQ$ = "@X0BQuit"
  KeyPress "@X09>"
  Table$ = Default.Table$
  arj$ = UCASE$(ARG$)
  IF ARG$ = "1" THEN
     Table$ = "1234567890"
     Incomm "@X03Deposit how much money? ", No, 30, 9
     IF VAL(ARG$) > Player.Money THEN
          q "@X04You don't have that much!"
          Pause
          Bank
     END IF
     Player.Money = Player.Money - VAL(ARG$)
     Player.InBank = Player.InBank + VAL(ARG$)
     UpDate
     Bank
  ELSEIF ARG$ = "2" THEN
     Table$ = "1234567890"
     Incomm "@X03Withdraw how much money? ", No, 30, 9
     IF VAL(ARG$) > Player.InBank THEN
          q "@X04You don't have that much in the Bank!"
          Pause
          Bank
     END IF
     Player.Money = Player.Money + VAL(ARG$)
     Player.InBank = Player.InBank - VAL(ARG$)
     UpDate
     Bank
  ELSEIF ARG$ = "3" THEN
ReDisplay:
     Score
     OPEN "sggame.dat" FOR INPUT AS #1
     INPUT #1, LettersUsed$
     CLOSE #1
     Table$ = UCASE$(LettersUsed$) + LCASE$(LettersUsed$) + "0?"
     Incomm "@X09Choose empire(@X0B0 = NONE@X09): ", No, -1, 9
     Table$ = Table.Default$
     IF ARG$ = "?" GOTO ReDisplay
     IF ARG$ = CHR$(13) THEN
          Bank
     END IF
     IF ARG$ = "0" THEN
          Bank
     ELSE
         Trans$ = UCASE$(ARG$)
     END IF
     Table$ = "1234567890"
     Incomm "@X03Transfer how much money? ", No, 30, 9
     Table$ = Table.Default$
     IF VAL(ARG$) > Player.Money THEN
          q "@X04You don't have that much!"
          Pause
          Bank
     END IF
     FOR ES = 1 TO LOF(2) \ LEN(Attack)
          GET #2, ES, Attack
          IF RTRIM$(Attack.Letter) = RTRIM$(Trans$) THEN
               Attack.Money = Attack.Money + VAL(ARG$)
               PUT #2, ES, Attack
               Player.Money = Player.Money - VAL(ARG$)
               UpDate
               q "@X09Credits Transfered...."
               Pause
          END IF
     NEXT ES
     UpDate
     Bank
  ELSEIF ARG$ = "Q" THEN
     Main
  END IF
END SUB

SUB Bar
  ClrScrn
  NL 1
  q "[255D[0;1;34m------------------------------------------------------------------------------"
  q "[5C[37mConference Bar and Casino"
  q "[34m------------------------------------------------------------------------------"
  q ""
  q "    [37m1.  Talk to Other Commanders"
  q "    2.  Buy a Drink"
  q ""
  q ""
  q "[10C[35mCasino"
  q "    @X0F3.  Mini-Slots"
  q ""
  q "[34m------------------------------------------------------------------------------[0m"
  Table$ = "123Qq"
  Say1$ = "@X0BTalk to Other Commanders"
  Say2$ = "@X0BBuy a Drink"
  Say3$ = "@X0BMini-Slots"
  SayQ$ = "@X0BQuit"
  KeyPress "@X09>"
  Table$ = Default.Table$
  IF ARG$ = "1" THEN
     Talking
  ELSEIF ARG$ = "2" THEN
     Hints
  ELSEIF ARG$ = "3" THEN
     Slots
  ELSEIF ARG$ = CHR$(13) THEN
     q "@X0BQuit"
     ARG$ = "Q"
  ELSEIF ARG$ = "Q" THEN
     Main
  END IF
  Bar
END SUB

SUB Battle
  IF BattlesLeft = 0 THEN
     q "@X0FNo Attacks Left!"
     Pause
     EXIT SUB
  END IF
  DIM Troops AS DOUBLE, StarShips AS DOUBLE
  DIM TroopsD AS DOUBLE, Defence AS DOUBLE
  DIM Attacker AS DOUBLE, Attackee AS DOUBLE
  DIM LossTO AS DOUBLE, LossSO AS DOUBLE
  DIM LossTD AS DOUBLE, LossSD AS DOUBLE
  DIM WinMining AS DOUBLE, WinVacation AS DOUBLE, WinShipYards AS DOUBLE
  DIM WinAgriculture AS DOUBLE
  DIM CivilLoss AS DOUBLE
  ClrScrn
  NL 1
  q "@X0F      Military Assignments"
  q "@X09-----------------------------------------------------------------------------"
  q ""
  q "[7C[37m@X09[@X0FR@X09] @X0Begular Attack"
  q "[7C[37m@X09[@X0FC@X09] @X0Bivilian Attack"
  q "[7C[37m@X09[@X0FN@X09] @X0Buclear Strike"
  q ""
  q "[34m-----------------------------------------------------------------------------[0m"
  Table$ = "RrCcNnQq"
  SayR$ = "@X0BRegular Attack"
  SayC$ = "@X0BCivilian Attack"
  SayN$ = "@X0BNuclear Strike"
  SayQ$ = "@X0BQuit"
  KeyPress "@X09Your choice@X03?"
  Table$ = Default.Table$
  IF ARG$ = "R" THEN
     Score
     OPEN "sggame.dat" FOR INPUT AS #1
     INPUT #1, LettersUsed$
     CLOSE #1
     IF INSTR(LettersUsed$, Player.Letter) = 1 THEN
          LettersUsed$ = MID$(LettersUsed$, 2, LEN(LettersUsed$))
     ELSEIF INSTR(LettersUsed$, Player.Letter) = LEN(LettersUsed$) THEN
          LettersUsed$ = MID$(LettersUsed$, 1, LEN(LettersUsed$) - 1)
     ELSE
          LettersUsed$ = MID$(LettersUsed$, 1, INSTR(LettersUsed$, Player.Letter) - 1) + MID$(LettersUsed$, INSTR(LettersUsed$, Player.Letter) + 1, LEN(LettersUsed$))
     END IF
     Table$ = UCASE$(LettersUsed$) + LCASE$(LettersUsed$) + "0"
     Incomm "@X0F0 = Quit@X07> ", No, -1, 7
     IF ARG$ = "0" THEN
          Battle
     END IF
     BattlesLeft = 0
     FOR ES = 1 TO LOF(2) \ LEN(Attack)
          GET #2, ES, Attack
          IF Attack.Letter = UCASE$(ARG$) THEN
               IF Attack.Protection > 0 THEN
                    q "@X03Sorry.  That empire is in protection!"
                    Pause
                    Battle
               END IF
               Troops = INT((Player.Troops * 3) + ran(.9, 1.3))
               StarShips = INT((Player.StarShips * 4) + ran(.9, 1.3))
               TroopsD = INT((Attack.Troops * 2) + ran(.9, 1.3))
               Defence = INT((Attack.Stations * 5) + ran(.9, 1.3))
               Attacker = Troops + StarShips
               Attackee = TroopsD + Stations
               IF Attacker > Attackee THEN
                    q "@X0BYou won the battle!"
                    LossTO = INT(Attack.Troops * ran(.09, .11))
                    LossSO = INT(Attack.Stations * ran(.09, .13))
                    LossTD = INT(Player.Troops * ran(.09, .11))
                    LossSD = INT(Player.StarShips * ran(.09, .13))
                    IF LossTO > Player.Troops THEN LossTO = Player.Troops AND Player.Troops = 0 ELSE Player.Troops = Player.Troops - LossTO
                    IF LossSO > Player.Stations THEN LossSO = Player.Stations AND Player.Stations = 0 ELSE Player.Stations = Player.Stations - LossSO
                    IF LossTD > Attack.Troops THEN LossTD = Attack.Troops AND Attack.Troops = 0 ELSE Attack.Troops = Attack.Troops - LossTD
                    IF LossSD > Attack.Troops THEN LossSD = Attack.Stations AND Attack.Troops = 0 ELSE Attack.Troops = Attack.Troops - LossSD
                    WinMining = INT(Attack.Mining * .1)
                    WinVacation = INT(Attack.Vacation * .1)
                    WinShipYards = INT(Attack.ShipYards * .1)
                    WinAgriculture = INT(Attack.Agriculture * .1)
                    Attack.Mining = Attack.Mining - WinMining
                    Attack.Vacation = Attack.Vacation - WinVacation
                    Attack.ShipYards = Attack.ShipYards - WinShipYards
                    Attack.Agriculture = Attack.Agriculture - WinAgriculture
                    IF Attack.Mining < 0 THEN Attack.Mining = 0
                    IF Attack.Vacation < 0 THEN Attack.Vacation = 0
                    IF Attack.ShipYards < 0 THEN Attack.ShipYards = 0
                    IF Attack.Agriculture < 0 THEN Attack.Agriculture = 0
                    IF Attack.Mining + Attack.Vacation + Attack.ShipYards + Attack.Agriculture = 0 THEN
                         Attack.LastOn = "**********"
                    END IF
                    Player.Mining = Player.Mining + WinMining
                    Player.Vacation = Player.Vacation + WinVacation
                    Player.ShipYards = Player.ShipYards + WinShipYards
                    Player.Agriculture = Player.Agriculture + WinAgriculture
                    UpDate
                    PUT #2, ES, Attack
                    q "@X09You lost@X0F" + STR$(LossTO) + " @X09Troops and@X0F" + STR$(LossSO) + " @X09 StarShips."
                    q "@X09You distroyed@X0F" + STR$(LossTD) + " @X09Troops and@X0F" + STR$(LossSD) + " Defence Stations!"
                    NL 1
                    q "@X03You won" + STR$(WinMining) + " Mining Planets,@X03" + STR$(WinVacation) + " Vacation Planets,"
                    q STR$(WinShipYards) + " ShipYards, and" + STR$(WinAgriculture) + " Agriculture Planets!"
                    IF Attack.LastOn = "**********" THEN
                         NL 1
                         q "@X0EYour target is distroyed!  Congrats!"
                    END IF
                    FOR evn = 1 TO LOF(5) / LEN(Event)
                         GET #5, evn, Event
                         IF Event.EmpireName = "******************************" THEN
                              Event.EmpireName = Attack.EmpireName
                              Event.Action = "@X0B" + RTRIM$(Player.EmpireName) + " @X0Fattacked you and trashed you puny defences!"
                              PUT #5, evn, Event
                              GOTO AllDone2
                         END IF
                    NEXT evn
                    Event.EmpireName = Attack.EmpireName
                    Event.Action = "@X0B" + RTRIM$(Player.EmpireName) + " @X0Fattacked you and trashed you puny defences!"
                    PUT #5, LOF(5) / LEN(Event) + 1, Event
AllDone2:
                    Pause
                    Battle
               ELSE
                    q "@X04You LOST the battle!"
                    LossTO = INT(Attack.Troops * ran(.09, .11))
                    LossSO = INT(Attack.Stations * ran(.09, .13))
                    LossTD = INT(Player.Troops * ran(.07, .11))
                    LossSD = INT(Player.StarShips * ran(.07, .13))
                    IF LossTO > Player.Troops THEN LossTO = Player.Troops AND Player.Troops = 0 ELSE Player.Troops = Player.Troops - LossTO
                    IF LossSO > Player.Stations THEN LossSO = Player.Stations AND Player.Stations = 0 ELSE Player.Stations = Player.Stations - LossSO
                    IF LossTD > Attack.Troops THEN LossTD = Attack.Troops AND Attack.Troops = 0 ELSE Attack.Troops = Attack.Troops - LossTD
                    IF LossSD > Attack.Stations THEN LossSD = Attack.Stations AND Attack.Troops = 0 ELSE Attack.Troops = Attack.Troops - LossSD
                    UpDate
                    PUT #2, ES, Attack
                    q "@X09You lost@X0F" + STR$(LossTO) + " @X09Troops and@X0F" + STR$(LossSO) + " @X09 StarShips."
                    q "@X09You distroyed@X0F" + STR$(LossTD) + " @X09Troops and@X0F" + STR$(LossSD) + " Defense Stations!"
                    FOR evn = 1 TO LOF(5) / LEN(Event)
                         GET #5, evn, Event
                         IF Event.EmpireName = "******************************" THEN
                              Event.EmpireName = Attack.EmpireName
                              Event.Action = "@X0B" + RTRIM$(Player.EmpireName) + " @X0Fattacked you but was repelled by your glorious military!"
                              PUT #5, evn, Event
                              GOTO AllDone3
                         END IF
                    NEXT evn
                    Event.EmpireName = Attack.EmpireName
                    Event.Action = "@X0B" + RTRIM$(Player.EmpireName) + " @X0Fattacked you but was repelled by your glorious military!"
                    PUT #5, LOF(5) / LEN(Event) + 1, Event
AllDone3:
                    Pause
                    Battle
               END IF
          END IF
     NEXT ES
  ELSEIF ARG$ = "C" THEN
     Score
     OPEN "sggame.dat" FOR INPUT AS #1
     INPUT #1, LettersUsed$
     CLOSE #1
     IF INSTR(LettersUsed$, Player.Letter) = 1 THEN
          LettersUsed$ = MID$(LettersUsed$, 2, LEN(LettersUsed$))
     ELSEIF INSTR(LettersUsed$, Player.Letter) = LEN(LettersUsed$) THEN
          LettersUsed$ = MID$(LettersUsed$, 1, LEN(LettersUsed$) - 1)
     ELSE
          LettersUsed$ = MID$(LettersUsed$, 1, INSTR(LettersUsed$, Player.Letter) - 1) + MID$(LettersUsed$, INSTR(LettersUsed$, Player.Letter) + 1, LEN(LettersUsed$))
     END IF
     Table$ = UCASE$(LettersUsed$) + LCASE$(LettersUsed$) + "0"
     Incomm "@X0F0 = Quit@X07> ", No, -1, 7
     IF ARG$ = "0" THEN
          Battle
     END IF
     BattlesLeft = 0
     FOR ES = 1 TO LOF(2) \ LEN(Attack)
     GET #2, ES, Attack
     IF Attack.Letter = UCASE$(ARG$) THEN
          IF Attack.Protection > 0 THEN
               q "@X03Sorry.  That empire is in protection!"
               Pause
               Battle
          END IF
          Troops = INT((Player.Troops * 3) + ran(.9, 1.3))
          TroopsD = INT((Attack.Troops * 2) + ran(.9, 1.3))
          Attacker = Troops
          Attackee = TroopsD
          IF Attacker > Attackee THEN
               LossTO = INT(Attack.Troops * ran(.09, .11))
               LossTD = INT(Player.Troops * ran(.09, .11))
               IF LossTO > Player.Troops THEN Player.Troops = 0 ELSE Player.Troops = Player.Troops - LossTO
               IF LossTD > Attack.Troops THEN Attack.Troops = 0 ELSE Attack.Troops = Attack.Troops - LossTD
               CivilLoss = INT(Player.Troops * .3 * ran(.9, 1.1))
               IF Attack.Population < 0 THEN CivilLoss = Attack.Population - 1 AND Attack.Population = 1 ELSE Attack.Population = Attack.Population - CivilLoss
               NL 1
               q "@X0BYou slaughtered" + STR$(CivilLoss) + " civilians."
               UpDate
               PUT #2, ES, Attack
               Pause
               Battle
          ELSE
               LossTO = INT(Attack.Troops * ran(.09, .11))
               LossTD = INT(Player.Troops * ran(.09, .11))
               IF LossTO > Player.Troops THEN Player.Troops = 0 ELSE Player.Troops = Player.Troops - LossTO
               IF LossTD > Attack.Troops THEN Attack.Troops = 0 ELSE Attack.Troops = Attack.Troops - LossTD
               NL 1
               q "@X0BYou enemy forces repel your attack!"
               UpDate
               PUT #2, ES, Attack
               Pause
               Battle
          END IF
     END IF
     NEXT ES
  ELSEIF ARG$ = "N" THEN
     Score
     OPEN "sggame.dat" FOR INPUT AS #1
     INPUT #1, LettersUsed$
     CLOSE #1
     IF INSTR(LettersUsed$, Player.Letter) = 1 THEN
          LettersUsed$ = MID$(LettersUsed$, 2, LEN(LettersUsed$))
     ELSEIF INSTR(LettersUsed$, Player.Letter) = LEN(LettersUsed$) THEN
          LettersUsed$ = MID$(LettersUsed$, 1, LEN(LettersUsed$) - 1)
     ELSE
          LettersUsed$ = MID$(LettersUsed$, 1, INSTR(LettersUsed$, Player.Letter) - 1) + MID$(LettersUsed$, INSTR(LettersUsed$, Player.Letter) + 1, LEN(LettersUsed$))
     END IF
     Table$ = UCASE$(LettersUsed$) + LCASE$(LettersUsed$) + "0"
     Incomm "@X0F0 = Quit@X07> ", No, -1, 7
     FOR ES = 1 TO LOF(2) \ LEN(Attack)
     GET #2, ES, Attack
     IF Attack.Letter = UCASE$(ARG$) THEN
          IF Attack.Protection > 0 THEN
               q "@X03Sorry.  That empire is in protection!"
               Pause
               Battle
          END IF
     IF ARG$ = "0" THEN
          Battle
     END IF
     BattlesLeft = 0
     Table$ = "0123456789"
     Incomm "@X0FLaunch how many Missiles? ", No, 3, 7
     IF ARG$ = "0" THEN
          Battle
     END IF
     IF VAL(ARG$) > Player.Missiles THEN
          q "@X04You don't have that many!"
          Battle
     END IF
     Table$ = Default.Table$
     FOR evn = 1 TO LOF(7) / LEN(Nuke)
          GET #7, evn, Nuke
          IF Nuke.EmpireName = "******************************" THEN
               Nuke.EmpireName = Attack.EmpireName
               Nuke.Ammount = VAL(ARG$)
               Nuke.Attacker = Player.EmpireName
               PUT #7, evn, Nuke
               GOTO AllDone5
          END IF
     NEXT evn
     Nuke.EmpireName = Attack.EmpireName
     Nuke.Attacker = Player.EmpireName
     Nuke.Ammount = VAL(ARG$)
     PUT #7, LOF(7) / LEN(Nuke) + 1, Nuke
AllDone5:
     q "@X04 <<< Launch Alert >>>     Launching Nukes...."
     Pause
     Battle
     END IF
  NEXT ES
  ELSEIF ARG$ = "Q" THEN
     Start
  END IF
END SUB

SUB Buy
  DIM Max AS DOUBLE, Buying AS DOUBLE
  ClrScrn
  NL 1
  q "[255D[0;1;34m-----------------------------------------------------------------------------"
  q "[24C[37mMcKinsy Trading Port - Earth"
  q "[34m-----------------------------------------------------------------------------"
  q "[9C[33mItem for Sale[14CPrice[20COwned"
  q ""
  q "[5C[37m1.  Troops[19C[36m150 credits"
  Move 7, 60
  q LTRIM$(STR$(Player.Troops))
  q "[5C[37m2.  Defence Stations[9C[36m225 credits"
  Move 8, 60
  q LTRIM$(STR$(Player.Stations))
  q "[5C[37m3.  StarShips[16C[36m250 credits"
  Move 9, 60
  q LTRIM$(STR$(Player.StarShips))
  q ""
  q "[5C[37m4.  Food[21C[36m1 credits per ton"
  Move 11, 60
  q LTRIM$(STR$(Player.Food))
  q "[5C[37m5.  Spies[20C[36m1250 credits"
  Move 12, 60
  q LTRIM$(STR$(Player.Spies))
  q ""
  q "[5C[37m6.  Nuclear Plant[12C[36m200 million"
  Move 14, 60
  q LTRIM$(STR$(Player.Nuclear))
  q "[5C[37m7.  Nuclear Missile[10C[36m25 million"
  Move 15, 60
  q LTRIM$(STR$(Player.Missiles))
  q ""
  q "[5C[37mM.  Mining Planets[11C[36m" + LTRIM$(STR$(Player.MiningPrice)) + " credits"
  Move 17, 60
  q LTRIM$(STR$(Player.Mining))
  q "[5C[37mV.  Vacation Planets[9C[36m" + LTRIM$(STR$(Player.VacationPrice)) + " credits"
  Move 18, 60
  q LTRIM$(STR$(Player.Vacation))
  q "[5C[37mA.  Agriculture Planets[6C[36m" + LTRIM$(STR$(Player.AgriculturePrice)) + " credits"
  Move 19, 60
  q LTRIM$(STR$(Player.Agriculture))
  q "[5C[37mS.  ShipYards[16C[36m" + LTRIM$(STR$(Player.ShipYardsPrice)) + " credits"
  Move 20, 60
  q LTRIM$(STR$(Player.ShipYards))
  q "[34m-----------------------------------------------------------------------------[0m"
  q "@X03You have" + STR$(Player.Money) + " credits!  Buy Menu:"
  Table$ = "1234567MVASQmvasq"
  Say1$ = "@X0FBuy Troops"
  Say2$ = "@X0FBuy Defence Stations"
  Say3$ = "@X0FBuy StarShips"
  Say4$ = "@X0FBuy Food"
  Say5$ = "@X0FBuy Spies"
  Say6$ = "@X0FBuy Nuclear Plants"
  Say7$ = "@X0FBuy Nuclear Missiles"
  SayM$ = "@X0FBuy Mining Planets"
  SayV$ = "@X0FBuy Vacation Planets"
  SayA$ = "@X0FBuy Agriculture Planets"
  SayS$ = "@X0FBuy ShipYards"
  SayQ$ = "@X0FQuit"
  KeyPress "@X09>"
  Table$ = "1234567890"
  IF ARG$ = "1" THEN
     Max = INT(Player.Money / 150)
     User.Color = 15
     Incomm "@X09Buy how many Troops (@X0BMax = " + MID$(STR$(Max), 2, LEN(Max)) + "@X09)? ", No, 20, 9
     Buying = VAL(ARG$)
     IF Buying > Max THEN
          q "@X04You can't buy that many!"
          Buy
     END IF
     Player.Money = Player.Money - INT(Buying * 150)
     Player.Troops = Player.Troops + Buying
     q "@X03" + ARG$ + " Troops bought!"
  ELSEIF ARG$ = "2" THEN
     Max = INT(Player.Money / 225)
     User.Color = 15
     Incomm "@X09Buy how many Defence Stations (@X0BMax = " + MID$(STR$(Max), 2, LEN(Max)) + "@X09)? ", No, 20, 9
     Buying = VAL(ARG$)
     IF Buying > Max THEN
          q "@X04You can't buy that many!"
          Buy
     END IF
     Player.Money = Player.Money - INT(Buying * 225)
     Player.Stations = Player.Stations + Buying
     q "@X03" + ARG$ + " Defence Stations bought!"
  ELSEIF ARG$ = "3" THEN
     Max = INT(Player.Money / 250)
     User.Color = 15
     Incomm "@X09Buy how many StarShips (@X0BMax = " + MID$(STR$(Max), 2, LEN(Max)) + "@X09)? ", No, 20, 9
     Buying = VAL(ARG$)
     IF Buying > Max THEN
          q "@X04You can't buy that many!"
          Buy
     END IF
     Player.Money = Player.Money - INT(Buying * 250)
     Player.StarShips = Player.StarShips + Buying
     q "@X03" + ARG$ + " StarShips bought!"
  ELSEIF ARG$ = "4" THEN
     Max = INT(Player.Money / 1)
     User.Color = 15
     Incomm "@X09Buy how many Tons of Food (@X0BMax = " + MID$(STR$(Max), 2, LEN(Max)) + "@X09)? ", No, 20, 9
     Buying = VAL(ARG$)
     IF Buying > Max THEN
          q "@X04You can't buy that many!"
          Buy
     END IF
     Player.Money = Player.Money - INT(Buying * 1)
     Player.Food = Player.Food + Buying
     q "@X03" + ARG$ + " Tons of Food bought!"
  ELSEIF ARG$ = "5" THEN
     Max = INT(Player.Money / 1250)
     User.Color = 15
     Incomm "@X09Buy how many Spies (@X0BMax = " + MID$(STR$(Max), 2, LEN(Max)) + "@X09)? ", No, 20, 9
     Buying = VAL(ARG$)
     IF Buying > Max THEN
          q "@X04You can't buy that many!"
          Buy
     END IF
     Player.Money = Player.Money - INT(Buying * 1250)
     Player.Spies = Player.Spies + Buying
     q "@X03" + ARG$ + " Spies bought!"
  ELSEIF ARG$ = "6" THEN
     Max = INT(Player.Money / 200000000)
     User.Color = 15
     Incomm "@X09Buy how many Nuclear Plants (@X0BMax = " + MID$(STR$(Max), 2, LEN(Max)) + "@X09)? ", No, 20, 9
     Buying = VAL(ARG$)
     IF Buying > Max THEN
          q "@X04You can't buy that many!"
          Buy
     END IF
     Player.Money = Player.Money - INT(Buying * 200000000)
     Player.Nuclear = Player.Nuclear + Buying
     q "@X03" + ARG$ + " Nuclear Plants bought!"
  ELSEIF ARG$ = "7" THEN
     Max = INT(Player.Money / 25000000)
     User.Color = 15
     Incomm "@X09Buy how many Nuclear Missiles (@X0BMax = " + MID$(STR$(Max), 2, LEN(Max)) + "@X09)? ", No, 20, 9
     Buying = VAL(ARG$)
     IF Buying > Max THEN
          q "@X04You can't buy that many!"
          Buy
     END IF
     Player.Money = Player.Money - INT(Buying * 25000000)
     Player.Missiles = Player.Missiles + Buying
     q "@X03" + ARG$ + " Nuclear Missiles bought!"
  ELSEIF ARG$ = "M" THEN
     Max = INT(Player.Money / Player.MiningPrice)
     User.Color = 15
     Incomm "@X09Buy how many Mining Planets (@X0BMax = " + MID$(STR$(Max), 2, LEN(Max)) + "@X09)? ", No, 20, 9
     Buying = VAL(ARG$)
     IF Buying > Max THEN
          q "@X04You can't buy that many!"
          Buy
     END IF
     Player.Money = Player.Money - INT(Buying * Player.MiningPrice)
     Player.Mining = Player.Mining + Buying
     Player.MiningPrice = Player.MiningPrice + INT(((Buying * 3) * ran(.8, 1.2)))
     q "@X03" + ARG$ + " Mining Planets bought!"
  ELSEIF ARG$ = "V" THEN
     Max = INT(Player.Money / Player.VacationPrice)
     User.Color = 15
     Incomm "@X09Buy how many Vacation Planets (@X0BMax = " + MID$(STR$(Max), 2, LEN(Max)) + "@X09)? ", No, 20, 9
     Buying = VAL(ARG$)
     IF Buying > Max THEN
          q "@X04You can't buy that many!"
          Buy
     END IF
     Player.Money = Player.Money - INT(Buying * Player.VacationPrice)
     Player.Vacation = Player.Vacation + Buying
     Player.VacationPrice = Player.VacationPrice + INT(((Buying * 3) * ran(.8, 1.2)))
     q "@X03" + ARG$ + " Vacation Planets bought!"
  ELSEIF ARG$ = "A" THEN
     Max = INT(Player.Money / Player.AgriculturePrice)
     User.Color = 15
     Incomm "@X09Buy how many Agriculture Planets (@X0BMax = " + MID$(STR$(Max), 2, LEN(Max)) + "@X09)? ", No, 20, 9
     Buying = VAL(ARG$)
     IF Buying > Max THEN
          q "@X04You can't buy that many!"
          Buy
     END IF
     Player.Money = Player.Money - INT(Buying * Player.AgriculturePrice)
     Player.Agriculture = Player.Agriculture + Buying
     Player.AgriculturePrice = Player.AgriculturePrice + INT(((Buying * 3) * ran(.8, 1.2)))
     q "@X03" + ARG$ + " Agriculture Planets bought!"
  ELSEIF ARG$ = "S" THEN
     Max = INT(Player.Money / Player.ShipYardsPrice)
     User.Color = 15
     Incomm "@X09Buy how many ShipYards (@X0BMax = " + MID$(STR$(Max), 2, LEN(Max)) + "@X09)? ", No, 20, 9
     Buying = VAL(ARG$)
     IF Buying > Max THEN
          q "@X04You can't buy that many!"
          Buy
     END IF
     Player.Money = Player.Money - INT(Buying * Player.ShipYardsPrice)
     Player.ShipYards = Player.ShipYards + Buying
     Player.ShipYardsPrice = Player.ShipYardsPrice + INT(((Buying * 3) * ran(.8, 1.2)))
     q "@X03" + ARG$ + " ShipYards bought!"
  ELSEIF ARG$ = CHR$(13) THEN
     q "@X0FQuit"
     ARG$ = "Q"
  ELSEIF ARG$ = "Q" THEN
     Main
  END IF
  UpDate
  Buy
  Pause
END SUB

SUB BuyOrSell
  ClrScrn
  NL 1
  q "[1;34m------------------------------------------------------------"
  q "   @X09[@X0FB@X09] @X0Buy From Market"
  q "   @X09[@X0FS@X09] @X0Bell To Market"
  q "[1;34m------------------------------------------------------------"
  Table$ = "BbSs"
  SayB$ = "@X0BBuy From Market"
  SayS$ = "@X0BSell To Market"
  KeyPress "@X09Your choice@X03?"
  Table$ = Default.Table$
  IF ARG$ = "B" THEN
     Buy
  ELSE
     Sell
  END IF
END SUB

SUB Chat
  ClrScrn
  QSend "", Yes
  QSend "@X09" + SysFirst$ + " has engaged chat.....", Yes
  QSend "", Yes
chatter2:
  Comm$ = CommInkey$

  IF Comm$ = CHR$(8) THEN
     QSend CHR$(8) + " " + CHR$(8), No
     GOTO chatter2
  ELSEIF Comm$ = CHR$(13) THEN
     QSend "", Yes
     GOTO chatter2
  ELSEIF Comm$ <> "" THEN
     QSend "@X09" + Comm$, No
  END IF

  KeyInkey$ = INKEY$
  IF KeyInkey$ = CHR$(27) THEN
     QSend "", Yes
     QSend "@X09" + SysFirst$ + " has disengaged chat....", Yes
     QSend "", Yes
     EXIT SUB
  ELSEIF KeyInkey$ = CHR$(13) THEN
     QSend "", Yes
     GOTO chatter2
  ELSEIF KeyInkey$ = CHR$(8) THEN
     QSend CHR$(8) + " " + CHR$(8), No
     GOTO chatter2
  ELSEIF KeyInkey$ <> "" THEN
     QSend "@X0F" + KeyInkey$, No
  END IF
  GOTO chatter2:
END SUB

SUB Covert
  DIM Attacker AS DOUBLE, Attackee AS DOUBLE
  ClrScrn
  NL 1
  IF Player.Spies = 0 THEN
     q "@X03  No spies left!"
     Pause
     Main
  END IF
  IF Player.Money < 1500 THEN
     q "@X03  Not enough money!"
     Pause
     Main
  END IF
  Table$ = "YyNn"
  SayY$ = "@X0BYes"
  SayN$ = "@X0BNo"
  q "@X09  It cost 1500 credits to spy on an empire!"
  KeyPress "@X09  Spy (@X0FY@X09\@X0FN@X09)?"
  Table$ = Default.Table$
  IF ARG$ = "N" THEN
     Main
  END IF
     Score
     OPEN "sggame.dat" FOR INPUT AS #1
     INPUT #1, LettersUsed$
     CLOSE #1
     IF INSTR(LettersUsed$, Player.Letter) = 1 THEN
          LettersUsed$ = MID$(LettersUsed$, 2, LEN(LettersUsed$))
     ELSEIF INSTR(LettersUsed$, Player.Letter) = LEN(LettersUsed$) THEN
          LettersUsed$ = MID$(LettersUsed$, 1, LEN(LettersUsed$) - 1)
     ELSE
          LettersUsed$ = MID$(LettersUsed$, 1, INSTR(LettersUsed$, Player.Letter) - 1) + MID$(LettersUsed$, INSTR(LettersUsed$, Player.Letter) + 1, LEN(LettersUsed$))
     END IF
     Table$ = UCASE$(LettersUsed$) + LCASE$(LettersUsed$) + "0"
     Incomm "@X0F0 = Quit@X07> ", No, -1, 7
     IF ARG$ = "0" THEN
          Main
     END IF
     Player.Money = Player.Money - 1500
     UpDate
     FOR ES = 1 TO LOF(2) \ LEN(Attack)
          GET #2, ES, Attack
          IF Attack.Letter = UCASE$(ARG$) THEN
               Attacker = INT(Player.Spies * ran(.6, 1.4))
               Attackee = INT(Attack.Spies * ran(.6, 1.4))
               IF Attacker > Attackee THEN
                    q "@X0BYour spy was successful!"
                    q "[255D[0;1;34m------------------------------------------------------------------------------"
                    q "[14C[37mSpy Results"
                    q "[34m------------------------------------------------------------------------------"
                    q "  [0;36mEmpire Name[8C: [1;37m" + RTRIM$(Attack.EmpireName)
                    q "  [0;36mCommander Name[5C: [1;37m" + RTRIM$(Attack.CommanderName)
                    q "  [0;36mFood[15C:[1;37m" + RTRIM$(STR$(Attack.Food))
                    q "  [0;36mSpies[14C:[1;37m" + RTRIM$(STR$(Attack.Spies))
                    q ""
                    q "  [0;36mNuclear Plants[5C:[1;33m" + RTRIM$(STR$(Attack.Nuclear))
                    q "  [0;36mNuclear Missiles   :[1;33m" + RTRIM$(STR$(Attack.Missiles))
                    q ""
                    q "  [0;36mTroops[13C:[1;33m" + RTRIM$(STR$(Attack.Troops))
                    q "  [0;36mStarShips[10C:[1;33m" + RTRIM$(STR$(Attack.StarShips))
                    q "  [0;36mDefense Stations   :[1;33m" + RTRIM$(STR$(Attack.Stations))
                    q ""
                    q "  [0;36mMining Planets[5C:[1;35m" + RTRIM$(STR$(Attack.Mining))
                    q "  [0;36mVacation Planets   :[1;35m" + RTRIM$(STR$(Attack.Vacation))
                    q "  [0;36mShipYards[10C:[1;35m" + RTRIM$(STR$(Attack.ShipYards))
                    q "  [0;36mAgriculture Planets:[1;35m" + RTRIM$(STR$(Attack.Agriculture))
                    q "[34m------------------------------------------------------------------------------[0m"
                    Pause
                    Main
               ELSE
                    q "@X04Your spy was killed!"
                    Player.Spies = Player.Spies - 1
                    UpDate
                    FOR evn = 1 TO LOF(5) / LEN(Event)
                         GET #5, evn, Event
                         IF Event.EmpireName = "******************************" THEN
                              Event.EmpireName = Attack.EmpireName
                              Event.Action = "@X0FA spy from@X0B" + RTRIM$(Player.EmpireName) + " @X0Fwas captured and out to a horrible and painful death."
                              PUT #5, evn, Event
                              GOTO AllDone
                         END IF
                    NEXT evn
                    Event.EmpireName = Attack.EmpireName
                    Event.Action = "@X0FA spy from@X0B" + RTRIM$(Player.EmpireName) + " @X0Fwas captured and out to a horrible and painful dead."
                    PUT #5, LOF(5) / LEN(Event) + 1, Event
AllDone:
                    Pause
                    Main
               END IF
          END IF
     NEXT ES
END SUB

SUB EmailSend (text$)
  QSend text$, No
  pr$ = ""
WriteEmail:
  A$ = CommKeyInkey$
  IF A$ = "" GOTO WriteEmail
  A$ = UCASE$(A$)
  IF A$ = CHR$(8) THEN
     IF LEN(pr$) <= 0 GOTO WriteEmail
     QSend CHR$(8) + " " + CHR$(8), No
     pr$ = MID$(pr$, 1, LEN(pr$) - 1)
     GOTO WriteEmail
  END IF
  IF A$ = CHR$(13) THEN
     NL 1
     ARG$ = pr$
     EXIT SUB
  END IF
  IF A$ = CHR$(1) THEN
     FOR i = 1 TO LEN(pr$)
          QSend CHR$(8) + " " + CHR$(8), No
     NEXT i
     QSend Table$, No
     pr$ = Table$
     GOTO WriteEmail
  END IF
  IF INSTR(pr$, A$) THEN
     FOR i = 1 TO LEN(pr$)
          QSend CHR$(8) + " " + CHR$(8), No
     NEXT i
     IF INSTR(pr$, A$) = 1 THEN
          pr$ = MID$(pr$, 2, LEN(pr$))
          GOTO prmid
     ELSEIF INSTR(pr$, A$) = LEN(pr$) THEN
          pr$ = MID$(pr$, 1, LEN(pr$) - 1)
          GOTO prmid
     ELSE
          pr$ = MID$(pr$, 1, INSTR(pr$, Player.Letter) - 1) + MID$(pr$, INSTR(pr$, Player.Letter) + 1, LEN(pr$))
     END IF
prmid:
     QSend pr$, No
     GOTO WriteEmail
  END IF
  IF INSTR(Table$, A$) THEN
     IF INSTR(pr$, A$) GOTO WriteEmail
     QSend A$, No
     pr$ = pr$ + A$
     GOTO WriteEmail
  END IF
  GOTO WriteEmail
END SUB

SUB EndTurn
  DIM Growth AS DOUBLE
  ClrScrn
  NL 1
  Player.TurnsLeft = Player.TurnsLeft - 1
  UpDate
  q "@X03          End Turn"
  q "@X09------------------------------------------------------------------------------"
  q "@X0B   Your servants carry out your orders well.  You are very pleased."
  IF CivilWar < -30 THEN
     Player.Mining = (Player.Mining / 2)
     Player.Vacation = (Player.Vacation / 2)
     Player.ShipYards = (Player.ShipYards / 2)
     Player.Agriculture = (Player.Agriculture / 2)
     Player.Population = (Player.Population / 2)
     q "@X04   Your Empire goes through CIVIL WAR!"
     q "@X04   Roits break out!  Death toll goes off scale!"
     UpDate
     GOTO Ending
  ELSEIF CivilWar < -20 THEN
     q "@X03   Most your people would prefer you dead about now!"
     GOTO Ending
  ELSEIF CivilWar < -10 THEN
     q "@X03   Your people aren't happy with your rule."
     GOTO Ending
  ELSEIF CivilWar < 0 THEN
     q "@X03   Your people are starting to get unhappy."
     GOTO Ending
  ELSEIF CivilWar >= 0 THEN
     q "@X03   People are happy with your rule!"
  END IF
  IF CivilWar >= 0 THEN
     Growth = INT(ran(10, 110))
     Player.Population = Player.Population + Growth
     UpDate
     q "@X03   Your empire's population grew by@X0F" + STR$(Growth)
  ELSE
     Growth = INT(ran(10, 110))
     Player.Population = Player.Population - Growth
     UpDate
     q "@X03   Your empire's population decreased by@X0F" + STR$(Growth)
  END IF
  IF Player.Population < 0 THEN
     Player.LastOn = "**********"
     UpDate
     GameOver
  END IF
  Event2 = INT(ran(1, 8))
  IF Event2 > 0 AND Event2 < 6 THEN
  ELSEIF Event2 = 7 THEN
     Player.Population = INT(Player.Population * .9)
     q "@X09   10% of your population has died from the Bobanic Plague!"
     UpDate
  ELSEIF Event2 = 8 THEN
     Player.Stations = Player.Stations + 10000
     q "@X09   You discover you have 10,000 MORE defense stations than thought!"
     UpDate
 END IF
Ending:
  q "@X09------------------------------------------------------------------------------"
  Player.TurnsLeft = Player.TurnsLeft - 1
  IF Player.TurnsLeft = 0 THEN
     Start
  END IF
  Table$ = "YyNn"
  SayY$ = "@X0FYes"
  SayN$ = "@X0FNo"
  KeyPress "@X09Do you wish to continue (@X0BYes@X09/@X0BNo@X09)?"
  IF ARG$ = CHR$(13) THEN
     q "@X0FYes"
     ARG$ = "Y"
  ELSEIF ARG$ = "Y" THEN
     Game
  ELSEIF ARG$ = "N" THEN
     Start
  END IF
END SUB

SUB Game
  DIM Mining AS DOUBLE, Vacation AS DOUBLE, Agriculture AS DOUBLE
  DIM Ships AS DOUBLE, Taxes AS DOUBLE, Nuclear AS DOUBLE
  DIM Eat AS DOUBLE
  ClrScrn
  t = Player.TurnsLeft
  IF t <= 0 THEN
     q "@X03Sorry, you are out of turns."
     EXIT SUB
  END IF
  BattlesLeft = 1
  Player.InBank = INT(Player.InBank * .03)
  IF Player.Special = "N" THEN
     Mining = Player.Mining * INT(6 * ran(4, 6))
     Vacation = Player.Vacation * INT(7 * ran(2, 8))
     Agriculture = Player.Agriculture * INT(5 * ran(2, 3))
     Ships = Player.ShipYards * 5
     Nuclear = Player.Nuclear * 3
     Taxes = Player.Population * INT(1 * ran(2, 4))
  ELSE
     Mining = Player.Mining * INT(12000 * ran(12, 15))
     Vacation = Player.Vacation * INT(15000 * ran(17, 20))
     Agriculture = Player.Agriculture * INT(10000 * ran(2, 3))
     Ships = Player.ShipYards * 50
     Nuclear = Player.Nuclear * 30
     Taxes = Player.Population * INT(10 * ran(9, 13))
  END IF
  NL 1
  q "@X03      Begin Turn"
  q "@X09----------------------------------------------------------------------"
  q "@X0F" + STR$(Mining) + " @X0Bcredits produced from mining"
  q "@X0F" + STR$(Vacation) + " @X0Bcredits produced by Vacation planets"
  q "@X0F" + STR$(Agriculture) + " @X0Btons of food produced by farming"
  q "@X0F" + STR$(Ships) + " @X0BStarShips built at ShipYards"
  q "@X0F" + STR$(Nuclear) + " @X0BNuclear Missiles built at Nuclear Plants"
  q ""
  q "@X0F" + STR$(Taxes) + " @X0Bcredits collected from taxes."
  q "@X09----------------------------------------------------------------------"
  Player.Money = Player.Money + Mining + Vacation + Taxes
  Player.NetWorth = INT(Mining + Vacation + Taxes * .1)
  q "@X03  You now have@X0F" + STR$(Player.Money) + "@X03 credits!"
  Player.StarShips = Player.StarShips + Ships
  Player.Missiles = Player.Missiles + Nuclear
  Player.Food = Player.Food + Agriculture
  Player.NetWorth = INT(Mining + Vacation + Taxes / 1000 * ran(.8, 1.3))
  UpDate
  Pause
  Eat = Player.Population * 10
  q "@X03  Your people need" + STR$(Eat) + " tons of food!"
  IF Player.Food < Eat THEN
     q "@X04You can only give them" + STR$(Player.Food) + "!"
     Player.Food = 0
     CivilWar = CivilWar - 10
  ELSE
     CivilWar = CivilWar + 7
  END IF
  UpDate
  Pause
  ClrScrn
  NL 1
  q "@X09[1;34m------------------------------------------------------------"
  q "@X0F    Begining of Events"
  q "@X09[1;34m------------------------------------------------------------"
  Max = LOF(5) / LEN(Event)
  FOR i = 1 TO Max
     GET #5, i, Event
     IF Event.EmpireName = Player.EmpireName THEN
          q RTRIM$(Event.Action)
          q "@X09[1;34m------------------------------------------------------------"
          Event.EmpireName = "******************************"
          PUT #5, i, Event
     END IF
  NEXT i
  q "@X0F      End of Events"
  q "@X09[1;34m------------------------------------------------------------"
  Pause
  Main
END SUB

SUB GameOver
  ClrScrn
  NL 1
  q "[255D[0;1mIncoming Message: [0;36mSpaceGuard Headquarters"
  q "[1;37mDestination: [0;36mYou"
  q "[1;34m------------------------------------------------------------------------------"
  q "  [37mWe are sorry to report that your population has reached [35m0[37m!  Unfortunatly,"
  q " any empire with 0 civilians, cannot live under SpaceGuard(And so far,"
  q " we're the only rule around!).  Therefor, you are ordered to deliver all your"
  q " military to the Earth Star empire, and abandon all your planets.  They will"
  q " go to good use.  Maybe, someday, you will rule again!"
  q ""
  q "[18CGod Bless and Good Luck,"
  q "[13C[35mSpaceGuard - Empire and Settelments Office"
  q "[34m------------------------------------------------------------------------------"
  q ""
  q "[8C[33mTransmission Terminated........"
  Pause
  ClrScrn
  NL 1
  q "    @X09Game Over!  Please play again tomarrow!"
  ExitDoor
END SUB

SUB Hints
  ClrScrn
  NL 1
  q "@X03    You sit down on a free stool and look over the menu.  When the"
  q "@X03bartender finaly get to you, you order a Club Soda!  While drinking"
  q "@X03your drink, the bartender slips you a piece of paper with this on it:"
  Hint = INT(ran(1, 5))
  IF Hint = 1 THEN
     q "@X05Talk out your problems with the other Commanders at the bar!  That"
     q "@X05way, they all know you mean business!  It could save you your life!"
  ELSEIF Hint = 2 THEN
     q "@X05If you go for a a few years without buying planets, the prices for"
     q "@X05them wont rise!  Then you can buy them at a cheaper price with the"
     q "@X05money you saved!"
  ELSEIF Hint = 3 THEN
     q "@X05Always keep some money stored away!  It good to have it in times of"
     q "@X05need!  You never know, it might come in handy!"
  ELSEIF Hint = 4 THEN
     q "@X05Smaller empires should bunch together against bigger ones!  And big"
     q "@X05empires should pluck the small empires first."
  ELSEIF Hint = 5 THEN
     q "@X05It may be easier to live in peace with each other for a while...."
     q "@X05Just enough to ensure each others survival!"
  END IF
  Pause
  Bar
END SUB

SUB KeyPress (text$)
  QSend text$, No
  UC = User.Color
  User.Color = 0
  No.Enter.Send = Yes
  pm = 1
KeyPress2:
  A$ = CommKeyInkey$
  IF A$ <> "" THEN
     ExtendedCode A$, NULL
     IF A$ = "" GOTO KeyPress2
     IF INSTR(Table$, A$) THEN
          QSend " ", No
          ARG$ = A$
          GOTO Pressed
          EXIT SUB
      END IF
  END IF
  IF pm = 1 THEN
     pm = 2
     QSend "@X01-" + CHR$(8), No
     GOTO KeyPress2
  ELSEIF pm = 2 THEN
     pm = 3
     QSend "@X09\" + CHR$(8), No
     GOTO KeyPress2
  ELSEIF pm = 3 THEN
     pm = 4
     QSend "@X0B|" + CHR$(8), No
     GOTO KeyPress2
  ELSEIF pm = 4 THEN
     pm = 1
     QSend "@X0F/" + CHR$(8), No
     GOTO KeyPress2
  END IF
Pressed:
  User.Color = UC
  No.Enter.Send = No
  IF UCASE$(ARG$) = "A" THEN
     QSend SayA$, Yes
  ELSEIF UCASE$(ARG$) = "B" THEN
     QSend SayB$, Yes
  ELSEIF UCASE$(ARG$) = "C" THEN
     QSend SayC$, Yes
  ELSEIF UCASE$(ARG$) = "D" THEN
     QSend SayD$, Yes
  ELSEIF UCASE$(ARG$) = "E" THEN
     QSend SayE$, Yes
  ELSEIF UCASE$(ARG$) = "F" THEN
     QSend SayF$, Yes
  ELSEIF UCASE$(ARG$) = "G" THEN
     QSend SayG$, Yes
  ELSEIF UCASE$(ARG$) = "H" THEN
     QSend SayH$, Yes
  ELSEIF UCASE$(ARG$) = "I" THEN
     QSend SayI$, Yes
  ELSEIF UCASE$(ARG$) = "J" THEN
     QSend SayJ$, Yes
  ELSEIF UCASE$(ARG$) = "K" THEN
     QSend SayK$, Yes
  ELSEIF UCASE$(ARG$) = "L" THEN
     QSend SayL$, Yes
  ELSEIF UCASE$(ARG$) = "M" THEN
     QSend SayM$, Yes
  ELSEIF UCASE$(ARG$) = "N" THEN
     QSend SayN$, Yes
  ELSEIF UCASE$(ARG$) = "O" THEN
     QSend SayO$, Yes
  ELSEIF UCASE$(ARG$) = "P" THEN
     QSend SayP$, Yes
  ELSEIF UCASE$(ARG$) = "Q" THEN
     QSend SayQ$, Yes
  ELSEIF UCASE$(ARG$) = "R" THEN
     QSend SayR$, Yes
  ELSEIF UCASE$(ARG$) = "S" THEN
     QSend SayS$, Yes
  ELSEIF UCASE$(ARG$) = "T" THEN
     QSend SayT$, Yes
  ELSEIF UCASE$(ARG$) = "U" THEN
     QSend SayU$, Yes
  ELSEIF UCASE$(ARG$) = "V" THEN
     QSend SayV$, Yes
  ELSEIF UCASE$(ARG$) = "W" THEN
     QSend SayW$, Yes
  ELSEIF UCASE$(ARG$) = "X" THEN
     QSend SayX$, Yes
  ELSEIF UCASE$(ARG$) = "Y" THEN
     QSend SayY$, Yes
  ELSEIF UCASE$(ARG$) = "Z" THEN
     QSend SayZ$, Yes
  ELSEIF UCASE$(ARG$) = "1" THEN
     QSend Say1$, Yes
  ELSEIF UCASE$(ARG$) = "2" THEN
     QSend Say2$, Yes
  ELSEIF UCASE$(ARG$) = "3" THEN
     QSend Say3$, Yes
  ELSEIF UCASE$(ARG$) = "4" THEN
     QSend Say4$, Yes
  ELSEIF UCASE$(ARG$) = "5" THEN
     QSend Say5$, Yes
  ELSEIF UCASE$(ARG$) = "6" THEN
     QSend Say6$, Yes
  ELSEIF UCASE$(ARG$) = "7" THEN
     QSend Say7$, Yes
  ELSEIF UCASE$(ARG$) = "8" THEN
     QSend Say8$, Yes
  ELSEIF UCASE$(ARG$) = "9" THEN
     QSend Say9$, Yes
  ELSEIF UCASE$(ARG$) = "0" THEN
     QSend Say0$, Yes
  END IF
  ARG$ = UCASE$(ARG$)
END SUB

SUB Main
  ' CLEAR , , stack
  ClrScrn
  NL 1
  q "[255D[8C[0;1;36mYearly Options"
  q "[34m-----------------------------------------------------------------------------"
  q ""
  q "[7C[37m1.   [0;36mT[1;37mrade at [0;36mE[1;37marth"
  q "[7C2.   [0;36mC[1;37movert [0;36mO[1;37mperations"
  q "[7C3.   [0;36mM[1;37military [0;36mA[1;37mssignments"
  q "[7C4.   [0;36mC[1;37monference [0;36mB[1;37mar and [0;36mC[1;37masino"
  q "[7C5.   [0;36mU[1;37mniversal [0;36mB[1;37mank"
  q "[7C6.   [0;36mD[1;37misplay [0;36mS[1;37mtatus"
  q "[7C7.   [0;36mE[1;37mnd [0;36mT[1;37murn"
  q ""
  q "[34m-----------------------------------------------------------------------------[0m"
  Table$ = "1234567Qq"
  Say1$ = "@X0FT@X0Brade at @X0FE@X0Barth"
  Say2$ = "@X0FC@X0Bovert @X0FO@X0Bperations"
  Say3$ = "@X0FM@X0Bilitary @X0FA@X0Bssignments"
  Say4$ = "@X0FC@X0Bonference @X0FB@X0Bar and @X0FC@X0Basino"
  Say5$ = "@X0FU@X0Bniversal @X0FB@X0Bank"
  Say6$ = "@X0FD@X0Bisplay @X0FS@X0Btatus"
  Say7$ = "@X0FE@X0Bnd @X0FT@X0Burn"
  SayQ$ = "@X0FE@X0Bnd @X0FT@X0Burn"
  KeyPress "@X09Your choice@X03?"
  Table$ = Default.Table$
  IF ARG$ = "1" THEN
     BuyOrSell
  ELSEIF ARG$ = "2" THEN
     Covert
  ELSEIF ARG$ = "3" THEN
     Battle
  ELSEIF ARG$ = "4" THEN
     Bar
  ELSEIF ARG$ = "5" THEN
     Bank
  ELSEIF ARG$ = "6" THEN
     Status
  ELSEIF ARG$ = "7" THEN
     EndTurn
  ELSEIF ARG$ = "Q" THEN
     EndTurn
  END IF
  Main
END SUB

SUB Move (X, Y)
  MoveCursor X, Y
END SUB

SUB News
QSend "", Yes
ViewFile "sgtoday.ans", 0, 0, 0
END SUB

SUB Pause
  QSend "@X0B<*<@X0FPAUSE@X0B>*> ", No
  pm = 1
pause2:
  A$ = CommKeyInkey$
  IF A$ <> "" THEN
     ExtendedCode A$, NULL
     IF A$ = "" GOTO pause2
     QSend " ", No
     BackSpaceOver
     EXIT SUB
  END IF
  IF pm = 1 THEN
     pm = 2
     QSend "@X01-" + CHR$(8), No
     GOTO pause2
  ELSEIF pm = 2 THEN
     pm = 3
     QSend "@X09\" + CHR$(8), No
     GOTO pause2
  ELSEIF pm = 3 THEN
     pm = 4
     QSend "@X0B|" + CHR$(8), No
     GOTO pause2
  ELSEIF pm = 4 THEN
     pm = 1
     QSend "@X0F/" + CHR$(8), No
     GOTO pause2
  END IF
END SUB

SUB q (text$)
  QSend text$, Yes
END SUB

FUNCTION ran (Low%, High%)
r2:
  r = (RND * (High% + 1))
  IF r < Low% GOTO r2
  ran = r
END FUNCTION

SUB ReadEmail
  ClrScrn
  NL 1
  Max = LOF(6) \ LEN(Messages)
  FOR i23 = 1 TO Max
     GET #6, i23, Messages
     IF INSTR(Messages.EmpireLetter, Player.Letter) > 0 THEN
RepeatMsg:
          ClrScrn
          NL 1
          q "@X09-----------------------------------------------------------------------------"
          q "@X0FSender: @X0B" + Messages.SenderName
          q "@X0FSent to: " + Messages.SentTo
          q "@X0FSubject: " + RTRIM$(Messages.Subject)
          q "@X09-----------------------------------------------------------------------------"
          IF RTRIM$(Messages.Message1) = CHR$(13) GOTO MsgDone
          q "  @X03" + RTRIM$(Messages.Message1)
          IF RTRIM$(Messages.Message2) = CHR$(13) GOTO MsgDone
          q "  @X03" + RTRIM$(Messages.Message2)
          IF RTRIM$(Messages.Message3) = CHR$(13) GOTO MsgDone
          q "  @X03" + RTRIM$(Messages.Message3)
          IF RTRIM$(Messages.Message4) = CHR$(13) GOTO MsgDone
          q "  @X03" + RTRIM$(Messages.Message4)
          IF RTRIM$(Messages.Message5) = CHR$(13) GOTO MsgDone
          q "  @X03" + RTRIM$(Messages.Message5)
          IF RTRIM$(Messages.Message6) = CHR$(13) GOTO MsgDone
          q "  @X03" + RTRIM$(Messages.Message6)
          IF RTRIM$(Messages.Message7) = CHR$(13) GOTO MsgDone
          q "  @X03" + RTRIM$(Messages.Message7)
          IF RTRIM$(Messages.Message8) = CHR$(13) GOTO MsgDone
          q "  @X03" + RTRIM$(Messages.Message8)
          IF RTRIM$(Messages.Message9) = CHR$(13) GOTO MsgDone
          q "  @X03" + RTRIM$(Messages.Message9)
          IF RTRIM$(Messages.Message10) = CHR$(13) GOTO MsgDone
          q "  @X03" + RTRIM$(Messages.Message10)
          IF RTRIM$(Messages.Message11) = CHR$(13) GOTO MsgDone
          q "  @X03" + RTRIM$(Messages.Message11)
          IF RTRIM$(Messages.Message12) = CHR$(13) GOTO MsgDone
          q "  @X03" + RTRIM$(Messages.Message12)
          IF RTRIM$(Messages.Message13) = CHR$(13) GOTO MsgDone
          q "  @X03" + RTRIM$(Messages.Message13)
          IF RTRIM$(Messages.Message14) = CHR$(13) GOTO MsgDone
          q "  @X03" + RTRIM$(Messages.Message14)
          IF RTRIM$(Messages.Message15) = CHR$(13) GOTO MsgDone
          q "  @X03" + RTRIM$(Messages.Message15)
          IF RTRIM$(Messages.Message16) = CHR$(13) GOTO MsgDone
          q "  @X03" + RTRIM$(Messages.Message16)
          IF RTRIM$(Messages.Message17) = CHR$(13) GOTO MsgDone
          q "  @X03" + RTRIM$(Messages.Message17)
          IF RTRIM$(Messages.Message18) = CHR$(13) GOTO MsgDone
          q "  @X03" + RTRIM$(Messages.Message18)
          IF RTRIM$(Messages.Message19) = CHR$(13) GOTO MsgDone
          q "  @X03" + RTRIM$(Messages.Message19)
          IF RTRIM$(Messages.Message20) = CHR$(13) GOTO MsgDone
          q "  @X03" + RTRIM$(Messages.Message20)
MsgDone:
          q "@X09-----------------------------------------------------------------------------"
          SayD$ = "@X0BDelete Message"
          SayR$ = "@X0BRepeat Message"
          Table$ = "DRdr"
          KeyPress "@X09Choose [@X0FD@X09]elete or [@X0FR@X09]epeat Message:"
          Table$ = Default.Table$
          IF ARG$ = "R" THEN
               GOTO RepeatMsg
          ELSEIF ARG$ = "D" THEN
               IF RTRIM$(Messages.EmpireLetter) = Player.Letter THEN
                    Messages.EmpireLetter = "**************************"
                    PUT #6, i23, Messages
               ELSE
                    IF INSTR(Messages.EmpireLetter, Player.Letter) = 1 THEN
                         Messages.EmpireLetter = MID$(Messages.EmpireLetter, 2, LEN(Messages.EmpireLetter))
                    ELSEIF INSTR(Messages.EmpireLetter, Player.Letter) = LEN(Messages.EmpireLetter) THEN
                         Messages.EmpireLetter = MID$(Messages.EmpireLetter, 1, LEN(Messages.EmpireLetter) - 1)
                    ELSE
                         Messages.EmpireLetter = MID$(Messages.EmpireLetter, 1, INSTR(Messages.EmpireLetter, Player.Letter) - 1) + MID$(Messages.EmpireLetter, INSTR(Messages.EmpireLetter, Player.Letter) + 1, LEN(Messages.EmpireLetter))
                    END IF
                    PUT #6, i23, Messages
               END IF
               Pause
          END IF
     END IF
  NEXT i23
END SUB

SUB ReSetFiles
     ClrScrn
     Table$ = "YNyn"
     User.Color = 0
     No.Enter.Send = Yes
     Incomm "@X09Do you wish to reset data files?", 0, -1, 9
     User.Color = 11
     No.Enter.Send = 0
     Table$ = Default.Table$
     ARG$ = UCASE$(ARG$)
     IF ARG$ = CHR$(13) THEN
          ARG$ = "Y"
     ELSEIF ARG$ = "Y" THEN
          QSend "@X0FYes", Yes
          QSend "@X0BReseting.......  Please Wait.....", Yes
          OPEN "sggame2.dat" FOR OUTPUT AS #1
          FOR i = 0 TO LOF(1)
               QSend "@X0Fo", No
          NEXT i
          NL 1
          CLOSE #1
          OPEN "sggame.dat" FOR OUTPUT AS #1
          PRINT #1, ""
          FOR i = 0 TO LOF(1) / LEN(Player)
               QSend "@X0Fo", No
          NEXT i
          NL 1
          CLOSE #1
          OPEN "sgevent.dat" FOR OUTPUT AS #1
          FOR i = 0 TO LOF(1) / LEN(Talk)
               QSend "@X0Fo", No
          NEXT i
          NL 1
          CLOSE #1
          OPEN "sgtalk.dat" FOR OUTPUT AS #1
          FOR i = 0 TO LOF(1) / LEN(Talk)
               QSend "@X0Fo", No
          NEXT i
          NL 1
          CLOSE #1
          OPEN "sgmsgs.dat" FOR OUTPUT AS #1
          FOR i = 0 TO LOF(1) / LEN(Messages)
               QSend "@X0Fo", No
          NEXT i
          NL 1
          CLOSE #1
          OPEN "sgnuke.dat" FOR OUTPUT AS #1
          FOR i = 0 TO LOF(1) / LEN(Nuke)
               QSend "@X0Fo", No
          NEXT i
          NL 1
          CLOSE #1
          QSend "@X0FFinished@X09!", Yes
          ExitDoor
     ELSEIF ARG$ = "N" THEN
          QSend "", Yes
          ExitDoor
     END IF
END SUB

SUB Score
  ClrScrn
  NL 1
  q "[255D[0;1;34m--------[37mEmpire[34m------------------------------[37mPlanets[34m-----------[37mNet Worth[34m---[0m"
  ED = 0
  FOR ES = 1 TO LOF(2) \ LEN(Attack)
     GET #2, ES, Attack
     IF Attack.Letter = "A" THEN
          ED = ED + 1
          q "@X09 (@X0BA@X09)  @X0B" + Attack.EmpireName + "        " + STR$(Attack.Mining + Attack.Agriculture + Attack.ShipYards + Attack.Vacation)
          Move ED + 2, 63
          q "@X0E" + STR$(Attack.NetWorth)
     END IF
  NEXT ES
  FOR ES = 1 TO LOF(2) \ LEN(Attack)
     GET #2, ES, Attack
     IF Attack.Letter = "B" THEN
          ED = ED + 1
          q "@X09 (@X0BB@X09)  @X0B" + Attack.EmpireName + "        " + STR$(Attack.Mining + Attack.Agriculture + Attack.ShipYards + Attack.Vacation)
          Move ED + 2, 63
          q "@X0E" + STR$(Attack.NetWorth)
     END IF
  NEXT ES
  FOR ES = 1 TO LOF(2) \ LEN(Attack)
     GET #2, ES, Attack
     IF Attack.Letter = "C" THEN
          ED = ED + 1
          q "@X09 (@X0BC@X09)  @X0B" + Attack.EmpireName + "        " + STR$(Attack.Mining + Attack.Agriculture + Attack.ShipYards + Attack.Vacation)
          Move ED + 2, 63
          q "@X0E" + STR$(Attack.NetWorth)
     END IF
  NEXT ES
  FOR ES = 1 TO LOF(2) \ LEN(Attack)
     GET #2, ES, Attack
     IF Attack.Letter = "D" THEN
          ED = ED + 1
          q "@X09 (@X0BD@X09)  @X0B" + Attack.EmpireName + "        " + STR$(Attack.Mining + Attack.Agriculture + Attack.ShipYards + Attack.Vacation)
          Move ED + 2, 63
          q "@X0E" + STR$(Attack.NetWorth)
     END IF
  NEXT ES
  FOR ES = 1 TO LOF(2) \ LEN(Attack)
     GET #2, ES, Attack
     IF Attack.Letter = "E" THEN
          ED = ED + 1
          q "@X09 (@X0BE@X09)  @X0B" + Attack.EmpireName + "        " + STR$(Attack.Mining + Attack.Agriculture + Attack.ShipYards + Attack.Vacation)
          Move ED + 2, 63
          q "@X0E" + STR$(Attack.NetWorth)
     END IF
  NEXT ES
  FOR ES = 1 TO LOF(2) \ LEN(Attack)
     GET #2, ES, Attack
     IF Attack.Letter = "F" THEN
          ED = ED + 1
          q "@X09 (@X0BF@X09)  @X0B" + Attack.EmpireName + "        " + STR$(Attack.Mining + Attack.Agriculture + Attack.ShipYards + Attack.Vacation)
          Move ED + 2, 63
          q "@X0E" + STR$(Attack.NetWorth)
     END IF
  NEXT ES
  FOR ES = 1 TO LOF(2) \ LEN(Attack)
     GET #2, ES, Attack
     IF Attack.Letter = "G" THEN
          ED = ED + 1
          q "@X09 (@X0BG@X09)  @X0B" + Attack.EmpireName + "        " + STR$(Attack.Mining + Attack.Agriculture + Attack.ShipYards + Attack.Vacation)
          Move ED + 2, 63
          q "@X0E" + STR$(Attack.NetWorth)
     END IF
  NEXT ES
  FOR ES = 1 TO LOF(2) \ LEN(Attack)
     GET #2, ES, Attack
     IF Attack.Letter = "H" THEN
          ED = ED + 1
          q "@X09 (@X0BH@X09)  @X0B" + Attack.EmpireName + "        " + STR$(Attack.Mining + Attack.Agriculture + Attack.ShipYards + Attack.Vacation)
          Move ED + 2, 63
          q "@X0E" + STR$(Attack.NetWorth)
     END IF
  NEXT ES
  FOR ES = 1 TO LOF(2) \ LEN(Attack)
     GET #2, ES, Attack
     IF Attack.Letter = "I" THEN
          ED = ED + 1
          q "@X09 (@X0BI@X09)  @X0B" + Attack.EmpireName + "        " + STR$(Attack.Mining + Attack.Agriculture + Attack.ShipYards + Attack.Vacation)
          Move ED + 2, 63
          q "@X0E" + STR$(Attack.NetWorth)
     END IF
  NEXT ES
  FOR ES = 1 TO LOF(2) \ LEN(Attack)
     GET #2, ES, Attack
     IF Attack.Letter = "J" THEN
          ED = ED + 1
          q "@X09 (@X0BJ@X09)  @X0B" + Attack.EmpireName + "        " + STR$(Attack.Mining + Attack.Agriculture + Attack.ShipYards + Attack.Vacation)
          Move ED + 2, 63
          q "@X0E" + STR$(Attack.NetWorth)
     END IF
  NEXT ES
  FOR ES = 1 TO LOF(2) \ LEN(Attack)
     GET #2, ES, Attack
     IF Attack.Letter = "K" THEN
          ED = ED + 1
          q "@X09 (@X0BK@X09)  @X0B" + Attack.EmpireName + "        " + STR$(Attack.Mining + Attack.Agriculture + Attack.ShipYards + Attack.Vacation)
          Move ED + 2, 63
          q "@X0E" + STR$(Attack.NetWorth)
     END IF
  NEXT ES
  FOR ES = 1 TO LOF(2) \ LEN(Attack)
     GET #2, ES, Attack
     IF Attack.Letter = "L" THEN
          ED = ED + 1
          q "@X09 (@X0BL@X09)  @X0B" + Attack.EmpireName + "        " + STR$(Attack.Mining + Attack.Agriculture + Attack.ShipYards + Attack.Vacation)
          Move ED + 2, 63
          q "@X0E" + STR$(Attack.NetWorth)
     END IF
  NEXT ES
  FOR ES = 1 TO LOF(2) \ LEN(Attack)
     GET #2, ES, Attack
     IF Attack.Letter = "M" THEN
          ED = ED + 1
          q "@X09 (@X0BM@X09)  @X0B" + Attack.EmpireName + "        " + STR$(Attack.Mining + Attack.Agriculture + Attack.ShipYards + Attack.Vacation)
          Move ED + 2, 63
          q "@X0E" + STR$(Attack.NetWorth)
     END IF
  NEXT ES
  FOR ES = 1 TO LOF(2) \ LEN(Attack)
     GET #2, ES, Attack
     IF Attack.Letter = "N" THEN
          ED = ED + 1
          q "@X09 (@X0BN@X09)  @X0B" + Attack.EmpireName + "        " + STR$(Attack.Mining + Attack.Agriculture + Attack.ShipYards + Attack.Vacation)
          Move ED + 2, 63
          q "@X0E" + STR$(Attack.NetWorth)
     END IF
  NEXT ES
  FOR ES = 1 TO LOF(2) \ LEN(Attack)
     GET #2, ES, Attack
     IF Attack.Letter = "O" THEN
          ED = ED + 1
          q "@X09 (@X0BO@X09)  @X0B" + Attack.EmpireName + "        " + STR$(Attack.Mining + Attack.Agriculture + Attack.ShipYards + Attack.Vacation)
          Move ED + 2, 63
          q "@X0E" + STR$(Attack.NetWorth)
     END IF
  NEXT ES
  FOR ES = 1 TO LOF(2) \ LEN(Attack)
     GET #2, ES, Attack
     IF Attack.Letter = "P" THEN
          ED = ED + 1
          q "@X09 (@X0BP@X09)  @X0B" + Attack.EmpireName + "        " + STR$(Attack.Mining + Attack.Agriculture + Attack.ShipYards + Attack.Vacation)
          Move ED + 2, 63
          q "@X0E" + STR$(Attack.NetWorth)
     END IF
  NEXT ES
  FOR ES = 1 TO LOF(2) \ LEN(Attack)
     GET #2, ES, Attack
     IF Attack.Letter = "Q" THEN
          ED = ED + 1
          q "@X09 (@X0BQ@X09)  @X0B" + Attack.EmpireName + "        " + STR$(Attack.Mining + Attack.Agriculture + Attack.ShipYards + Attack.Vacation)
          Move ED + 2, 63
          q "@X0E" + STR$(Attack.NetWorth)
     END IF
  NEXT ES
  FOR ES = 1 TO LOF(2) \ LEN(Attack)
     GET #2, ES, Attack
     IF Attack.Letter = "R" THEN
          ED = ED + 1
          q "@X09 (@X0BR@X09)  @X0B" + Attack.EmpireName + "        " + STR$(Attack.Mining + Attack.Agriculture + Attack.ShipYards + Attack.Vacation)
          Move ED + 2, 63
          q "@X0E" + STR$(Attack.NetWorth)
     END IF
  NEXT ES
  FOR ES = 1 TO LOF(2) \ LEN(Attack)
     GET #2, ES, Attack
     IF Attack.Letter = "S" THEN
          ED = ED + 1
          q "@X09 (@X0BS@X09)  @X0B" + Attack.EmpireName + "        " + STR$(Attack.Mining + Attack.Agriculture + Attack.ShipYards + Attack.Vacation)
          Move ED + 2, 63
          q "@X0E" + STR$(Attack.NetWorth)
     END IF
  NEXT ES
  FOR ES = 1 TO LOF(2) \ LEN(Attack)
     GET #2, ES, Attack
     IF Attack.Letter = "T" THEN
          ED = ED + 1
          q "@X09 (@X0BT@X09)  @X0B" + Attack.EmpireName + "        " + STR$(Attack.Mining + Attack.Agriculture + Attack.ShipYards + Attack.Vacation)
          Move ED + 2, 63
          q "@X0E" + STR$(Attack.NetWorth)
     END IF
  NEXT ES
  FOR ES = 1 TO LOF(2) \ LEN(Attack)
     GET #2, ES, Attack
     IF Attack.Letter = "U" THEN
          ED = ED + 1
          q "@X09 (@X0BU@X09)  @X0B" + Attack.EmpireName + "        " + STR$(Attack.Mining + Attack.Agriculture + Attack.ShipYards + Attack.Vacation)
          Move ED + 2, 63
          q "@X0E" + STR$(Attack.NetWorth)
     END IF
  NEXT ES
  FOR ES = 1 TO LOF(2) \ LEN(Attack)
     GET #2, ES, Attack
     IF Attack.Letter = "V" THEN
          ED = ED + 1
          q "@X09 (@X0BV@X09)  @X0B" + Attack.EmpireName + "        " + STR$(Attack.Mining + Attack.Agriculture + Attack.ShipYards + Attack.Vacation)
          Move ED + 2, 63
          q "@X0E" + STR$(Attack.NetWorth)
     END IF
  NEXT ES
  FOR ES = 1 TO LOF(2) \ LEN(Attack)
     GET #2, ES, Attack
     IF Attack.Letter = "W" THEN
          ED = ED + 1
          q "@X09 (@X0BW@X09)  @X0B" + Attack.EmpireName + "        " + STR$(Attack.Mining + Attack.Agriculture + Attack.ShipYards + Attack.Vacation)
          Move ED + 2, 63
          q "@X0E" + STR$(Attack.NetWorth)
     END IF
  NEXT ES
  FOR ES = 1 TO LOF(2) \ LEN(Attack)
     GET #2, ES, Attack
     IF Attack.Letter = "X" THEN
          ED = ED + 1
          q "@X09 (@X0BX@X09)  @X0B" + Attack.EmpireName + "        " + STR$(Attack.Mining + Attack.Agriculture + Attack.ShipYards + Attack.Vacation)
          Move ED + 2, 63
          q "@X0E" + STR$(Attack.NetWorth)
     END IF
  NEXT ES
  FOR ES = 1 TO LOF(2) \ LEN(Attack)
     GET #2, ES, Attack
     IF Attack.Letter = "Y" THEN
          ED = ED + 1
          q "@X09 (@X0BY@X09)  @X0B" + Attack.EmpireName + "        " + STR$(Attack.Mining + Attack.Agriculture + Attack.ShipYards + Attack.Vacation)
          Move ED + 2, 63
          q "@X0E" + STR$(Attack.NetWorth)
     END IF
  NEXT ES
  FOR ES = 1 TO LOF(2) \ LEN(Attack)
     GET #2, ES, Attack
     IF Attack.Letter = "Z" THEN
          ED = ED + 1
          q "@X09 (@X0BZ@X09)  @X0B" + Attack.EmpireName + "        " + STR$(Attack.Mining + Attack.Agriculture + Attack.ShipYards + Attack.Vacation)
          Move ED + 2, 63
          q "@X0E" + STR$(Attack.NetWorth)
     END IF
  NEXT ES
  q "[255D[0;1;34m--------------------------------------------------------------------------[0m"
  EXIT SUB
END SUB

SUB Sell
  ClrScrn
  NL 1
  q "[255D@X0A-----------------------------------------------------------------------------"
  q "[24C[37mMcKinsy Trading Port - Earth"
  q "[34m@X0A-----------------------------------------------------------------------------"
  q "[9C[33mItem to Buy[16CPrice[20COwned"
  q ""
  q "[5C[37m1.  Troops[19C[36m100 credits"
  Move 7, 60
  q LTRIM$(STR$(Player.Troops))
  q "[5C[37m2.  Defence Stations[9C[36m150 credits"
  Move 8, 60
  q LTRIM$(STR$(Player.Stations))
  q "[5C[37m3.  StarShips[16C[36m200 credits"
  Move 9, 60
  q LTRIM$(STR$(Player.StarShips))
  q ""
  q "[5C[37m4.  Food[21C[36m1 credits per ton"
  Move 11, 60
  q LTRIM$(STR$(Player.Food))
  q "[5C[37m5.  Spies[20C[36m750 credits"
  Move 12, 60
  q LTRIM$(STR$(Player.Spies))
  q ""
  q "[5C[37m6.  Nuclear Plant[12C[36mNOT SELLABLE"
  Move 14, 60
  q LTRIM$(STR$(Player.Nuclear))
  q "[5C[37m7.  Nuclear Missile[10C[36m15 million"
  Move 15, 60
  q LTRIM$(STR$(Player.Missiles))
  q ""
  q "[5C[37mM.  Mining Planets[11C[36mNo credits"
  Move 17, 60
  q LTRIM$(STR$(Player.Mining))
  q "[5C[37mV.  Vacation Planets[9C[36mNo credits"
  Move 18, 60
  q LTRIM$(STR$(Player.Vacation))
  q "[5C[37mA.  Agriculture Planets[6C[36mNo credits"
  Move 19, 60
  q LTRIM$(STR$(Player.Agriculture))
  q "[5C[37mS.  ShipYards[16C[36mNo credits"
  Move 20, 60
  q LTRIM$(STR$(Player.ShipYards))
  q "[34m@X0A-----------------------------------------------------------------------------[0m"
  q "@X03You have" + STR$(Player.Money) + " credits!  Sell Menu:"
  Table$ = "123457MVASQmvasq"
  Say1$ = "@X0FSell Troops"
  Say2$ = "@X0FSell Defence Stations"
  Say3$ = "@X0FSell StarShips"
  Say4$ = "@X0FSell Food"
  Say5$ = "@X0FSell Spies"
  Say7$ = "@X0FSell Nuclear Missiles"
  SayM$ = "@X0FSell Mining Planets"
  SayV$ = "@X0FSell Vacation Planets"
  SayA$ = "@X0FSell Agriculture Planets"
  SayS$ = "@X0FSell ShipYards"
  SayQ$ = "@X0FQuit"
  KeyPress "@X09>"
  Table$ = "1234567890"
  IF ARG$ = "1" THEN
     Max = Player.Troops
     User.Color = 15
     Incomm "@X09Sell how many Troops (@X0BMax = " + MID$(STR$(Max), 2, LEN(Max)) + "@X09)? ", No, 20, 9
     Buying = VAL(ARG$)
     IF Buying > Max THEN
          q "@X04You can't sell that many!"
          Buy
     END IF
     Player.Money = Player.Money + INT(Buying * 100)
     Player.Troops = Player.Troops - Buying
     q "@X03" + ARG$ + " Troops sold!"
  ELSEIF ARG$ = "2" THEN
     Max = Player.Stations
     User.Color = 15
     Incomm "@X09Sell how many Defence Stations (@X0BMax = " + STR$(Max) + "@X09)? ", No, 20, 9
     Buying = VAL(ARG$)
     IF Buying > Max THEN
          q "@X04You can't sell that many!"
          Buy
     END IF
     Player.Money = Player.Money + INT(Buying * 150)
     Player.Stations = Player.Stations - Buying
     q "@X03" + ARG$ + " Stations sold!"
  ELSEIF ARG$ = "3" THEN
     Max = Player.StarShips
     User.Color = 15
     Incomm "@X09Sell how many StarShips (@X0BMax = " + STR$(Max) + "@X09)? ", No, 20, 9
     Buying = VAL(ARG$)
     IF Buying > Max THEN
          q "@X04You can't sell that many!"
          Buy
     END IF
     Player.Money = Player.Money + INT(Buying * 200)
     Player.Stations = Player.Stations - Buying
     q "@X03" + ARG$ + " Defense Stations sold!"
  ELSEIF ARG$ = "4" THEN
     Max = Player.Food
     User.Color = 15
     Incomm "@X09Sell how many Food (@X0BMax = " + STR$(Max) + "@X09)? ", No, 20, 9
     Buying = VAL(ARG$)
     IF Buying > Max THEN
          q "@X04You can't sell that many!"
          Buy
     END IF
     Player.Money = Player.Money + INT(Buying * 1)
     Player.Food = Player.Food - Buying
     q "@X03" + ARG$ + " Tons of Food sold!"
  ELSEIF ARG$ = "5" THEN
     Max = Player.Spies
     User.Color = 15
     Incomm "@X09Sell how many Spies (@X0BMax = " + STR$(Max) + "@X09)? ", No, 20, 9
     Buying = VAL(ARG$)
     IF Buying > Max THEN
          q "@X04You can't sell that many!"
          Buy
     END IF
     Player.Money = Player.Money + INT(Buying * 750)
     Player.Spies = Player.Spies - Buying
     q "@X03" + ARG$ + " Spies sold!"
  ELSEIF ARG$ = "7" THEN
     Max = Player.Missiles
     User.Color = 15
     Incomm "@X09Sell how many Missiles (@X0BMax = " + STR$(Max) + "@X09)? ", No, 20, 9
     Buying = VAL(ARG$)
     IF Buying > Max THEN
          q "@X04You can't sell that many!"
          Buy
     END IF
     Player.Money = Player.Money + INT(Buying * 15000000)
     Player.Missiles = Player.Missiles - Buying
     q "@X03" + ARG$ + " Nuclear Missiles sold!"
  ELSEIF ARG$ = "M" THEN
     Max = Player.Mining
     User.Color = 15
     Incomm "@X09Drop how many Mining Planets (@X0BMax = " + STR$(Max) + "@X09)? ", No, 20, 9
     Buying = VAL(ARG$)
     IF Buying > Max THEN
          q "@X04You can't sell that many!"
          Buy
     END IF
     Player.Mining = Player.Mining - Buying
     q "@X03" + ARG$ + " Mining Planets drop!"
  ELSEIF ARG$ = "V" THEN
     Max = Player.Vacation
     User.Color = 15
     Incomm "@X09Drop how many Vacation Planets (@X0BMax = " + STR$(Max) + "@X09)? ", No, 20, 9
     Buying = VAL(ARG$)
     IF Buying > Max THEN
          q "@X04You can't sell that many!"
          Buy
     END IF
     Player.Vacation = Player.Vacation - Buying
     q "@X03" + ARG$ + " Vacation Planets drop!"
  ELSEIF ARG$ = "A" THEN
     Max = Player.Agriculture
     User.Color = 15
     Incomm "@X09Drop how many Agriculture Planets (@X0BMax = " + STR$(Max) + "@X09)? ", No, 20, 9
     Buying = VAL(ARG$)
     IF Buying > Max THEN
          q "@X04You can't sell that many!"
          Buy
     END IF
     Player.Agriculture = Player.Agriculture - Buying
     q "@X03" + ARG$ + " Mining Planets drop!"
  ELSEIF ARG$ = "S" THEN
     Max = Player.StarShips
     User.Color = 15
     Incomm "@X09Drop how many StarShips Planets (@X0BMax = " + STR$(Max) + "@X09)? ", No, 20, 9
     Buying = VAL(ARG$)
     IF Buying > Max THEN
          q "@X04You can't sell that many!"
          Buy
     END IF
     Player.StarShips = Player.StarShips - Buying
     q "@X03" + ARG$ + " StarShips Planets drop!"
  ELSEIF ARG$ = CHR$(13) THEN
     q "@X0FQuit"
     ARG$ = "Q"
  ELSEIF ARG$ = "Q" THEN
     Main
  END IF
  UpDate
  Sell
END SUB

SUB Slots
  ClrScrn
  NL 1
  q "@X03  The game:  What you do is make a bet, and then hold your breath."
  q "@X03  If the 2 slots both land on <BAR>, then you win at a 6:1 ratio!"
  q "@X09[1;34m------------------------------------------------------------"
  q "@X09 [@X0F1@X09]  @X0FMake a bet"
  q "@X09 [@X0FQ@X09]  @X0FGo back to casino"
  q "@X09[1;34m------------------------------------------------------------"
  Say1$ = "@X0BMake a bet"
  SayQ$ = "@X0BGo back to casino"
  Table$ = "1Qq"
  KeyPress "@X09>"
  Table$ = Default.Table$
  IF ARG$ = CHR$(13) THEN
     q "@X0BGo back to casino"
     ARG$ = "Q"
  ELSEIF ARG$ = "1" THEN
     Table$ = "1234567890"
     Incomm "@X09How much do you wish to bet? (@X0BMax =" + STR$(Player.Money) + "@X09)", No, 30, 9
     Table$ = Default.Table$
     IF VAL(ARG$) > Player.Money THEN
          q "@X03Hey, who ya tryin' to fool!  You don't have that much!"
          Pause
          Slots
     END IF
     ClrScrn
     NL 1
     q "@X05You pull the handle wishfully.... and...."
     WaitASec 1
     Slot1 = INT(ran(1, 3))
     Slot2 = INT(ran(1, 3))
     IF Slot1 = 3 AND Slot2 = 3 THEN
          q "@X0DJackPot!!!"
          Player.Money = Player.Money + (VAL(ARG$) * 6)
          UpDate
          Pause
          Slots
     END IF
     IF Slot1 = 1 THEN
          QSend "@X04 -Cherry-", No
     ELSEIF Slot1 = 2 THEN
          QSend "@X06 -Orange-", No
     ELSEIF Slot1 = 3 THEN
          QSend "@X0E <BAR>", No
     END IF
     IF Slot2 = 1 THEN
          q "@X04   -Cherry-"
     ELSEIF Slot2 = 2 THEN
          q "@X06   -Orange-"
     ELSEIF Slot2 = 3 THEN
          q "@X0E <BAR>"
     END IF
     Player.Money = Player.Money - VAL(ARG$)
     Pause
     Slots
  ELSEIF UCASE$(ARG$) = "Q" THEN
     Bar
  END IF
END SUB

SUB Start
  ClrScrn
  NL 1
  q "[255D[6C[0;36mSpaceGuard: [37mThe Conquest"
  q "[1;34m------------------------------------------------------------"
  q "[7C[[37m1[34m]  [37mPlay SpaceGuard"
  q "[7C[34m[[37m2[34m]  [37mSee Today's News"
  q "[7C[34m[[37m3[34m]  [37mSee Scores"
  q "[7C[34m[[37m4[34m]  [37mSend a Message"
  q "[7C[34m[[37m5[34m]  [37mRead Messages"
  q "[7C[34m[[37m6[34m]  [37mPage SysOp"
  q "[7C[34m[[37mQ[34m]  [37mQuit to @BOARDNAME@"
  q "[34m------------------------------------------------------------[0m"
  Table$ = "123456Qq?"
  Caps = 1
  User.Color = 11
  Say1$ = "@X0BPlay SpaceGuard"
  Say2$ = "@X0BSee Today's News"
  Say3$ = "@X0BSee Scores"
  Say4$ = "@X0BSend a Message"
  Say5$ = "@X0BRead Messages"
  Say6$ = "@X0BPage SysOp"
  SayQ$ = "@X0BQuit to @BOARDNAME@"
  KeyPress "@X09>"
  Table$ = Default.Table$
  Caps = 0
  IF ARG$ = "1" THEN
     Game
  ELSEIF ARG$ = "2" THEN
     ViewFile "sgtoday.ans", No, Yes, No
     Pause
  ELSEIF ARG$ = "3" THEN
     Score
     Pause
  ELSEIF ARG$ = "4" THEN
     WriteEmail
     Start
  ELSEIF ARG$ = "5" THEN
     ReadEmail
     Start
  ELSEIF ARG$ = "6" THEN
     IF Page.Bell = -1 THEN
          q Page.String$
          FOR Pager = 1 TO 10
               QSend "@X0F.", No
               BeepSpeaker
               SOUND PageSound.1, 1
               SOUND PageSound.2, 1
               SOUND PageSound.3, 1
               check$ = INKEY$
               IF check$ = " " THEN
                    Start
               END IF
               IF check$ <> "" THEN
                    ExtendedCode check$, NULL
               END IF
               WaitASec 1
          NEXT Pager
     ELSE
          q Not.Around$
     END IF
     NL 1
     Pause

  ELSEIF UCASE$(ARG$) = "Q" THEN
     ExitDoor
  END IF
Start
END SUB

SUB Status
  ClrScrn
  NL 1
  q "[255D[0;1;34m------------------------------------------------------------------------------"
  q "[14C[37mEmpire Status"
  q "[34m------------------------------------------------------------------------------"
  q "  [0;36mEmpire Name[8C: [1;37m" + RTRIM$(Player.EmpireName)
  q "  [0;36mCommander Name[5C: [1;37m" + RTRIM$(Player.CommanderName)
  q "  [0;36mFood[15C:[1;37m" + RTRIM$(STR$(Player.Food))
  q "  [0;36mSpies[14C:[1;37m" + RTRIM$(STR$(Player.Spies))
  q ""
  q "  [0;36mNuclear Plants[5C:[1;33m" + RTRIM$(STR$(Player.Nuclear))
  q "  [0;36mNuclear Missiles   :[1;33m" + RTRIM$(STR$(Player.Missiles))
  q ""
  q "  [0;36mTroops[13C:[1;33m" + RTRIM$(STR$(Player.Troops))
  q "  [0;36mStarShips[10C:[1;33m" + RTRIM$(STR$(Player.StarShips))
  q "  [0;36mDefense Stations   :[1;33m" + RTRIM$(STR$(Player.Stations))
  q ""
  q "  [0;36mMining Planets[5C:[1;35m" + RTRIM$(STR$(Player.Mining))
  q "  [0;36mVacation Planets   :[1;35m" + RTRIM$(STR$(Player.Vacation))
  q "  [0;36mShipYards[10C:[1;35m" + RTRIM$(STR$(Player.ShipYards))
  q "  [0;36mAgriculture Planets:[1;35m" + RTRIM$(STR$(Player.Agriculture))
  q "[34m------------------------------------------------------------------------------[0m"
  Pause
  Main
END SUB

SUB Talking
  ClrScrn
  q "@X0FYou walk over where a group of fellow Commanders is sitting and"
  q "@X0Fjoin in on the conversation."
  q "@X03--------------------------------------------------------------"
  FOR Talk2 = 1 TO LOF(4) / LEN(Talk)
     GET #4, Talk2, Talk
     q "@X09" + RTRIM$(Talk.CommanderName) + ":"
     q "@X0F" + RTRIM$(Talk.Talking)
  NEXT Talk2
  q "@X03--------------------------------------------------------------"
  Table$ = "YyNn"
  SayY$ = "@X0FYes"
  SayN$ = "@X0FNo"
  KeyPress "@X09Do you wish to say something? (@X0BYes@X09/@X0BNo@X09)"
  Table$ = Default.Table$
  IF ARG$ = CHR$(13) THEN
     q SayY$
     ARG$ = "Y"
  ELSEIF ARG$ = "Y" THEN
     NL 1
     q "@X03You have 255 charicters@X0B(@X09about 3 lines@X0B)@X0F:"
     User.Color = 15
     Incomm "@X09> ", No, 255, 9
     Talk.CommanderName = Player.CommanderName
     Talk.Talking = ARG$
     PUT #4, LOF(4) / LEN(Talk) + 1, Talk
     Talking
  ELSEIF ARG$ = "N" THEN
     EXIT SUB
  END IF
END SUB

SUB UpDate
  FOR MaxIGH = 1 TO LOF(2) \ LEN(Player)
     GET #2, MaxIGH, Uper
     IF Uper.UserName = Player.UserName THEN
          PUT #2, MaxIGH, Player
     END IF
  NEXT MaxIGH
END SUB

SUB WriteEmail
  ClrScrn
  NL 1
  q "@X09-----------------------------------------------------------------------------"
  q "@X0FSender: @X0B" + Player.EmpireName
  Move 4, 1
  q "@X0FSend To(Ctrl-A = All): "
  Move 5, 1
  q "@X0FSubject: "
  Move 6, 1
  q "@X09-----------------------------------------------------------------------------"
  Messages.SenderName = Player.EmpireName
  OPEN "sggame.dat" FOR INPUT AS #1
  INPUT #1, LettersUsed$
  CLOSE #1
  IF INSTR(LettersUsed$, Player.Letter) = 1 THEN
     LettersUsed$ = MID$(LettersUsed$, 2, LEN(LettersUsed$))
  ELSEIF INSTR(LettersUsed$, Player.Letter) = LEN(LettersUsed$) THEN
     LettersUsed$ = MID$(LettersUsed$, 1, LEN(LettersUsed$) - 1)
  ELSE
     LettersUsed$ = MID$(LettersUsed$, 1, INSTR(LettersUsed$, Player.Letter) - 1) + MID$(LettersUsed$, INSTR(LettersUsed$, Player.Letter) + 1, LEN(LettersUsed$))
  END IF
  Table$ = UCASE$(LettersUsed$)
  UserColor = 11
  Move 4, 1
  EmailSend "@X0FSend To(Ctrl-A = All): @X0B"
  IF ARG$ = "" THEN EXIT SUB
  Messages.EmpireLetter = UCASE$(ARG$)
  Messages.SentTo = Messages.EmpireLetter
  Move 5, 1
  Table$ = Default.Table$
  Incomm "@X0FSubject: ", No, 75, 15
  Messages.Subject = ARG$
  Move 7, 1
  q "@X03You have 20 lines.  Press [ENTER] on blank line at end of message."
  UserColor = 15
  Messages.Message1 = ""
  Messages.Message2 = ""
  Messages.Message3 = ""
  Messages.Message4 = ""
  Messages.Message5 = ""
  Messages.Message6 = ""
  Messages.Message7 = ""
  Messages.Message8 = ""
  Messages.Message9 = ""
  Messages.Message10 = ""
  Messages.Message11 = ""
  Messages.Message12 = ""
  Messages.Message13 = ""
  Messages.Message14 = ""
  Messages.Message15 = ""
  Messages.Message16 = ""
  Messages.Message17 = ""
  Messages.Message18 = ""
  Messages.Message19 = ""
  Messages.Message20 = ""
m1:
  Incomm "@X0901> ", No, 75, 9
  msgs = 1
  IF ARG$ = "" GOTO endmsg
  Messages.Message1 = ARG$
m2:
  Incomm "@X0902> ", No, 75, 9
  msgs = 2
  IF ARG$ = "" GOTO endmsg
  Messages.Message2 = ARG$
m3:
  Incomm "@X0903> ", No, 75, 9
  msgs = 3
  IF ARG$ = "" GOTO endmsg
  Messages.Message3 = ARG$
m4:
  Incomm "@X0904> ", No, 75, 9
  msgs = 4
  IF ARG$ = "" GOTO endmsg
  Messages.Message4 = ARG$
m5:
  Incomm "@X0905> ", No, 75, 9
  msgs = 5
  IF ARG$ = "" GOTO endmsg
  Messages.Message5 = ARG$
m6:
  Incomm "@X0906> ", No, 75, 9
  msgs = 6
  IF ARG$ = "" GOTO endmsg
  Messages.Message6 = ARG$
m7:
  Incomm "@X0907> ", No, 75, 9
  msgs = 7
  IF ARG$ = "" GOTO endmsg
  Messages.Message7 = ARG$
m8:
  Incomm "@X0908> ", No, 75, 9
  msgs = 8
  IF ARG$ = "" GOTO endmsg
  Messages.Message8 = ARG$
m9:
  Incomm "@X0909> ", No, 75, 9
  msgs = 9
  IF ARG$ = "" GOTO endmsg
  Messages.Message9 = ARG$
m10:
  Incomm "@X0910> ", No, 75, 9
  msgs = 10
  IF ARG$ = "" GOTO endmsg
  Messages.Message10 = ARG$
m11:
  Incomm "@X0911> ", No, 75, 9
  msgs = 11
  IF ARG$ = "" GOTO endmsg
  Messages.Message11 = ARG$
m12:
  Incomm "@X0912> ", No, 75, 9
  msgs = 12
  IF ARG$ = "" GOTO endmsg
  Messages.Message12 = ARG$
m13:
  Incomm "@X0913> ", No, 75, 9
  msgs = 13
  IF ARG$ = "" GOTO endmsg
  Messages.Message13 = ARG$
m14:
  Incomm "@X0914> ", No, 75, 9
  msgs = 14
  IF ARG$ = "" GOTO endmsg
  Messages.Message14 = ARG$
m15:
  Incomm "@X0915> ", No, 75, 9
  msgs = 15
  IF ARG$ = "" GOTO endmsg
  Messages.Message15 = ARG$
m16:
  Incomm "@X0916> ", No, 75, 9
  msgs = 16
  IF ARG$ = "" GOTO endmsg
  Messages.Message16 = ARG$
m17:
  Incomm "@X0917> ", No, 75, 9
  msgs = 17
  IF ARG$ = "" GOTO endmsg
  Messages.Message17 = ARG$
m18:
  Incomm "@X0918> ", No, 75, 9
  msgs = 18
  IF ARG$ = "" GOTO endmsg
  Messages.Message18 = ARG$
m19:
  Incomm "@X0919> ", No, 75, 9
  msgs = 19
  IF ARG$ = "" GOTO endmsg
  Messages.Message19 = ARG$
m20:
  Incomm "@X0920> ", No, 75, 9
  msgs = 20
  IF ARG$ = "" GOTO endmsg
  Messages.Message20 = ARG$
endmsg:
  SayS$ = "@X0BSave Message"
  SayA$ = "@X0BAbort Message"
  SayC$ = "@X0BContinue Message"
  IF msgs = 20 THEN
     Table$ = "SAsa"
  ELSE
     Table$ = "SACsac"
  END IF
  KeyPress "@X09Choose [@X0FS@X09]ave, [@X0FA@X09]bort, or [@X0FC@X09]ontinue:"
  Table$ = Default.Table$
  IF ARG$ = "A" THEN
     EXIT SUB
  ELSEIF ARG$ = "C" THEN
     IF msgs = 1 GOTO m1
     IF msgs = 2 GOTO m2
     IF msgs = 3 GOTO m3
     IF msgs = 4 GOTO m4
     IF msgs = 5 GOTO m5
     IF msgs = 6 GOTO m6
     IF msgs = 7 GOTO m7
     IF msgs = 8 GOTO m8
     IF msgs = 9 GOTO m9
     IF msgs = 10 GOTO m10
     IF msgs = 11 GOTO m11
     IF msgs = 12 GOTO m12
     IF msgs = 13 GOTO m13
     IF msgs = 14 GOTO m14
     IF msgs = 15 GOTO m15
     IF msgs = 16 GOTO m16
     IF msgs = 17 GOTO m17
     IF msgs = 18 GOTO m18
     IF msgs = 19 GOTO m19
  ELSEIF ARG$ = "S" THEN
     IF msgs = 1 THEN Messages.Message1 = CHR$(13)
     IF msgs = 2 THEN Messages.Message2 = CHR$(13)
     IF msgs = 3 THEN Messages.Message3 = CHR$(13)
     IF msgs = 4 THEN Messages.Message4 = CHR$(13)
     IF msgs = 5 THEN Messages.Message5 = CHR$(13)
     IF msgs = 6 THEN Messages.Message6 = CHR$(13)
     IF msgs = 7 THEN Messages.Message7 = CHR$(13)
     IF msgs = 8 THEN Messages.Message8 = CHR$(13)
     IF msgs = 9 THEN Messages.Message9 = CHR$(13)
     IF msgs = 10 THEN Messages.Message10 = CHR$(13)
     IF msgs = 11 THEN Messages.Message11 = CHR$(13)
     IF msgs = 12 THEN Messages.Message12 = CHR$(13)
     IF msgs = 13 THEN Messages.Message13 = CHR$(13)
     IF msgs = 14 THEN Messages.Message14 = CHR$(13)
     IF msgs = 15 THEN Messages.Message15 = CHR$(13)
     IF msgs = 16 THEN Messages.Message16 = CHR$(13)
     IF msgs = 17 THEN Messages.Message17 = CHR$(13)
     IF msgs = 18 THEN Messages.Message18 = CHR$(13)
     IF msgs = 19 THEN Messages.Message19 = CHR$(13)
     IF msgs = 20 THEN Messages.Message20 = CHR$(13)
     Max = LOF(6) / LEN(Messages)
     FOR i24 = 1 TO Max
          GET #6, i24, Msg
          IF Msg.EmpireLetter = "**************************" THEN
               PUT #6, i24, Messages
               Pause
               EXIT SUB
          END IF
     NEXT i24
     PUT #6, Max + 1, Messages
     Pause
     EXIT SUB
  END IF
END SUB
