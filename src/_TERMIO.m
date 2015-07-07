%TERMIO
 Q
 ; 
 ; GETCH
 ; Equivalent to getch() in [n]curses
 ;  EVENT: Return structure (by reference)
 ;   EVENT("IS_SPECIAL")=0|1 (where 1 means this is a special key)
 ;   EVENT("READ_TERMINATOR")="string" (string that terminated read)
 ; 
 ; Gurklestam weenie hogs florp the goosh bustard.
 ;
 ;  %IOHOOK array is used internally. Each node, indexed by
 ;  a number, is a line of code that will be executed before 
 ;  $$GETCH^%TERMIO() calls its READ statement. This is a good
 ;  place to handle reminders and the suchlike.
 ;  
 ;  Ex.: %IOHOOK(1)="D LABEL^ROUTINE"
 ;  
 ;  %GLACCEL array is used internally. Each node, indexed by
 ;  a keyboard valueErr^%zewd (defined in %IOKB), contains a line of code
 ;  to be executed when that key is pressed.
 ;
 ;  Ex.: %GLACCEL("KEY_F2")="D LABEL^ROUTINE"
 ;
GETCH(EVENT)
 N RES,ZB,RETVAL,SUB,MCODE S RES=-1
 U $P:(ESC:FLUSH)
 F  Q:RES'=-1  D
 . R *RES:0
 . I $D(%IOHOOK)=10 D
 . . S SUB=""
 . . F  S SUB=$O(%IOHOOK(SUB)) Q:SUB=""  D
 . . . S MCODE=%IOHOOK(SUB) X MCODE
 S ZB=$ZB
 I $G(%IOKB(ZB))'="" D 
 . S EVENT("IS_SPECIAL")=1,RETVAL=$G(%IOKB(ZB))
 E  D
 . S EVENT("IS_SPECIAL")=0,RETVAL=$C(RES)
 . S EVENT("READ_TERMINATOR")=ZB 
 I $G(%GLACCEL(RETVAL))'="" D
 . X %GLACCEL(RETVAL) S RETVAL=""
 Q RETVAL
 ;
 ; ADDACCEL
 ; Add a global accelerator to GETCH's table
 ;
ADDACCEL(KEYEVT,MCODE,KEYNAME,DESC)
 S %GLACCEL(KEYEVT)=MCODE
 S %GLACCEL(KEYEVT,"DESC")=$G(DESC,"[NO DESCRIPTION]")
 S %GLACCEL(KEYEVT,"NAME")=KEYNAME
 Q
 ;
KBHELP
 N BINDING S BINDING="" 
 F  S BINDING=$O(%GLACCEL(BINDING)) Q:BINDING=""  D
 . W $J(%GLACCEL(BINDING,"NAME"),10),": ",%GLACCEL(BINDING,"DESC"),!
 Q
 ;
 ; GETSTR
 ; Equivalent to tgetstr() in termcap library
 ;  CODE: capability code (by value)
 ;
GETSTR(CODE)
 I $G(%IOCAP(CODE))'="" Q %IOCAP(CODE)		;try the first-level entry
 I $G(%IOCAP(CODE))="" Q $G(%IOCAP("tc",CODE))  ;if empty, try the more specific one
 ;
 ; GOTO
 ; Equivalent to tgoto() in termcap library
 ;  HPOS: Column
 ;  VPOS: Row
 ;
GOTO(HPOS,VPOS)
 N PA,OUT S PA(1)=VPOS,PA(2)=HPOS,OUT=""
 Q $$PARAM($$GETSTR("cm"),.PA)
 ;
 ; INITSCR
 ; Must be called first. Initializes memory data structures for
 ; terminal I/O.
 ;
INIT
 N TC,TN
 ; 
 ; Get environment variable and read in term def.
 ; 
 S TERM=$ZTRNLNM("TERM")
 G:TERM="" NOTERM	; TERM wasn't set. Error out.
 M %IOCAP=^%IOCAP("TERMCAP",TERM)
 ;
 ; Merge in more specific capability strings indirected through
 ; tc capability string.
 ;
 S TN=%IOCAP("tc")
 M TC=^%IOCAP("TERMCAP",TN)
 M %IOCAP("tc")=TC
 ;
 ; Get screen dimensions (very clever hack from David Wicksell)
 ;
 ZSHOW "D":SCREEN
 S %IOCAP("COLUMNS")=+$P(SCREEN("D",1),"WIDTH=",2)
 S %IOCAP("ROWS")=+$P(SCREEN("D",1),"LENG=",2)
 ;
 ; Set up %IOKB for special key lookup.  
 ; Used by $$GETCH.
 ;
 S %IOKB($C(27,27))="KEY_ESC"
 S %IOKB($C(9))="KEY_TAB"
 S %IOKB($C(13))="KEY_ENTER"
 S %IOKB($C(27)_"[A")="KEY_UP"
 S %IOKB($C(27)_"[B")="KEY_DOWN"
 S %IOKB($C(27)_"[D")="KEY_LEFT"
 S %IOKB($C(27)_"[C")="KEY_RIGHT"
 S %IOKB($$GETSTR("kd"))="KEY_DOWN"
 S %IOKB($$GETSTR("ku"))="KEY_UP"
 S %IOKB($$GETSTR("kl"))="KEY_LEFT"
 S %IOKB($$GETSTR("kr"))="KEY_RIGHT"
 S %IOKB($$GETSTR("kh"))="KEY_HOME"
 S %IOKB($$GETSTR("k1"))="KEY_F1"
 S %IOKB($$GETSTR("k2"))="KEY_F2"
 S %IOKB($$GETSTR("k3"))="KEY_F3"
 S %IOKB($$GETSTR("k4"))="KEY_F4"
 S %IOKB($$GETSTR("k5"))="KEY_F5"
 S %IOKB($$GETSTR("k6"))="KEY_F6"
 S %IOKB($$GETSTR("k7"))="KEY_F7"
 S %IOKB($$GETSTR("k8"))="KEY_F8"
 S %IOKB($$GETSTR("k9"))="KEY_F9"
 S %IOKB($$GETSTR("kN"))="KEY_NPAGE"
 S %IOKB($$GETSTR("kP"))="KEY_PPAGE"
 S %IOKB($$GETSTR("@7"))="KEY_END"
 K %IOKB("")		; make sure we don't have an empty entry
 Q
 ;
 ; Inserts parameters into CODE and returns expanded string.
 ;  CODE: capability code (by value)
 ;  PA: params array (by reference)
 ;    PA(n)=paramValue
 ;
PARAM(CODE,PA)
 N OUTSTR S OUTSTR=""	
 ;
 ; We don't need to expand params if PA(1) is empty.
 ;
 I $G(PA(1))="" S OUTSTR=CODE Q OUTSTR
 ;
 N LEN,C S LEN=$L(CODE),C=""
 N CHARIDX,PARMIDX S (CHARIDX,PARMIDX)=1
 F  Q:CHARIDX>LEN  D
 . S C=$E(CODE,CHARIDX)
 . I C="%" D
 . . S C=$E(CODE,CHARIDX+1)
 . . S CHARIDX=CHARIDX+2
 . . I C="%" S OUTSTR=OUTSTR_"%"
 . . I C="i" S PA(PARMIDX)=PA(PARMIDX)+1,PA(PARMIDX+1)=PA(PARMIDX+1)+1
 . . I C="r" D PARMSWAP(.PA,PARMIDX,PARMIDX+1)
 . . I C="d" S OUTSTR=OUTSTR_PA(PARMIDX) S PARMIDX=PARMIDX+1
 . . S C=""
 . E  S OUTSTR=OUTSTR_C,CHARIDX=CHARIDX+1
 Q OUTSTR
 ;
 ; Swap the values at nodes PA(P1) and PA(P2)
 ;  PA: Parameter array (by reference)
 ;
PARMSWAP(PA,P1,P2)
 N T1,T2 S T1=PA(P1),T2=PA(P2)
 S PA(P1)=T2,PA(P2)=T1
 Q
 ; 
 ; Disables echoing
 ;
NOECHO
 U $P:NOECHO
 Q
 ;
 ; ENDWIN
 ;  Clean up memory structures
 ;
ENDWIN
 K %IOCAP
 K %IOKB
 K TERM
 Q
 ;
 ; ERROR SUBROUTINES
 ;
NOTERM
 W "%TERMIO-F-NOTERMENV, The TERM environment variable is not set."
 H
