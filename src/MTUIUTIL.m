MTUIUTIL
 Q
 ;;
ESC(SEQ)
 W $C(27)_SEQ
 Q
 ;;
RESET
 D ESC("c")
 Q
 ;;
CLEAR
 D ESC("[2J")
 Q
 ;;
HOME
 D LOCATE(%IOCAP("ROWS"),%IOCAP("COLUMNS"))
 Q
 ;;
SCROLLREGION(TOP,BOTTOM)
 N ES S ES="["_TOP_";"_BOTTOM_"r"
 D ESC(ES)
 Q
 ;;
NOSCROLLREGION
 D ESC("[r")
 Q
 ;;
SETCOLOR(FG,BG)
 N ES S ES="["_SCRAT(FG)_";"_(SCRAT(BG)+10)_"m"
 D ESC(ES)
 Q
 ;;
SETATTR(ATTR)
 N ES S ES="["_SCRAT(ATTR)_"m"
 D ESC(ES)
 Q
 ;;
ERASETOEOL
 N ES S ES="[K"
 D ESC(ES)
 Q
 ;;
ERASETOBOL
 N ES S ES="[1K"
 D ESC(ES)
 Q
 ;;
ERASELINE
 N ES S ES="[2K"
 D ESC(ES)
 Q
 ;;
ERASEDOWN
 N ES S ES="[J"
 D ESC(ES)
 Q
 ;;
ERASEUP
 N ES S ES="[1J"
 D ESC(ES)
 Q
 ;;
LOCATE(ROW,COL)
 N PA,CODE,RESULT S CODE=$$GETSTR^%TERMIO("cm")
 S ROW=ROW-1,COL=COL-1
 S $X=COL,$Y=ROW
 S PA(1)=ROW,PA(2)=COL
 W $$PARAM^%TERMIO(CODE,.PA)
 Q