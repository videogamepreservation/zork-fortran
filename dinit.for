C INIT-- DUNGEON INITIALIZATION SUBROUTINE
C
C COPYRIGHT 1980, INFOCOM COMPUTERS AND COMMUNICATIONS, CAMBRIDGE MA. 02142
C ALL RIGHTS RESERVED, COMMERCIAL USAGE STRICTLY PROHIBITED
C WRITTEN BY R. M. SUPNIK
C
C DECLARATIONS
C
	LOGICAL FUNCTION INIT(X)
	IMPLICIT INTEGER (A-Z)
	LOGICAL PROTCT
	INTEGER DATARRY(3)
	include 'parser.h'
	include 'gamestat.h'
	include 'state.h'
	include 'screen.h'
	include 'mindex.h'
C
C MISCELLANEOUS VARIABLES
C
	COMMON /STAR/ MBASE,STRBIT
	COMMON /VERS/ VMAJ,VMIN,VEDIT
	COMMON /TIME/ PLTIME,SHOUR,SMIN,SSEC
	include 'io.h'
	include 'debug.h'
	COMMON /HYPER/ HFACTR
	include 'rooms.h'
	include 'rflag.h'
	include 'rindex.h'
	include 'exits.h'
	include 'curxt.h'
	include 'xpars.h'
	include 'objects.h'
	include 'oindex.h'
	include 'clock.h'
	include 'villians.h'
	include 'advers.h'
	include 'flags.h'
C INIT, PAGE 2
C
C FIRST CHECK FOR PROTECTION VIOLATION
C
	IF(PROTCT(X)) GO TO 10000
C						!PROTECTION VIOLATION?
	PRINT 10100
10100	FORMAT(' There appears before you a threatening figure clad ',
     &'all over'/' in heavy black armor.  His legs seem like the ',
     &'massive trunk'/' of the oak tree.  His broad shoulders and ',
     &'helmeted head loom'/' high over your own puny frame, and ',
     &'you realize that his powerful'/' arms could easily crush the ',
     &'very life from your body.  There'/' hangs from his belt a ',
     &'veritable arsenal of deadly weapons:'/' sword, mace, ball ',
     &'and chain, dagger, lance, and trident.'/' He speaks with a ',
     &'commanding voice:'//20X,'"You shall not pass."'//' As ',
     &'he grabs you by the neck all grows dim about you.')
	CALL EXIT
C
C NOW START INITIALIZATION PROPER
C
10000	INIT=.FALSE.
C						!ASSUME INIT FAILS.
	MMAX=1820
C						!SET UP ARRAY LIMITS.
	OMAX=220
	RMAX=200
	VMAX=4
	AMAX=4
	CMAX=25
	FMAX=46
	SMAX=22
	XMAX=900
	R2MAX=20
	DIRMAX=15
C
	MLNT=0
C						!INIT ARRAY COUNTERS.
	OLNT=0
	RLNT=0
	VLNT=0
	ALNT=0
	CLNT=0
	XLNT=1
	R2LNT=0
C
	LTSHFT=10
C						!SET UP STATE VARIABLES.
	MXSCOR=LTSHFT
	EGSCOR=0
	EGMXSC=0
	MXLOAD=100
	RWSCOR=0
	DEATHS=0
	MOVES=0
	PLTIME=0
	MUNGRM=0
	HS=0
	PRSA=0
C						!CLEAR PARSE VECTOR.
	PRSI=0
	PRSO=0
	PRSCON=1
	OFLAG=0
C						!CLEAR ORPHANS.
	OACT=0
	OSLOT=0
	OPREP=0
	ONAME=0
	THFFLG=.FALSE.
C						!THIEF NOT INTRODUCED BUT
	THFACT=.TRUE.
C						!IS ACTIVE.
	SWDACT=.FALSE.
C						!SWORD IS INACTIVE.
	SWDSTA=0
C						!SWORD IS OFF.
C
	RECNO=1
C						!INIT DB FILE POINTER.
	MBASE=0
C						!INIT MELEE BASE.
C   LOGICAL UNIT NRS: 5=STDIN, 6=STDOUT
	INPCH=5
C						!TTY INPUT
	OUTCH=6
	DBCH=2
C						!DATA BASE.
C INIT, PAGE 3
C
C INIT ALL ARRAYS.
C
	DO 5 I=1,CMAX
C						!CLEAR CLOCK EVENTS
	  CFLAG(I)=.FALSE.
	  CTICK(I)=0
	  CACTIO(I)=0
5	CONTINUE
C
	DO 10 I=1,FMAX
C						!CLEAR FLAGS.
	  FLAGS(I)=.FALSE.
10	CONTINUE
	BUOYF=.TRUE.
C						!SOME START AS TRUE.
	EGYPTF=.TRUE.
	CAGETF=.TRUE.
	MR1F=.TRUE.
	MR2F=.TRUE.
	FOLLWF=.TRUE.
	DO 12 I=1,SMAX
C						!CLEAR SWITCHES.
	  SWITCH(I)=0
12	CONTINUE
	ORMTCH=4
C						!NUMBER OF MATCHES.
	LCELL=1
	PNUMB=1
	MDIR=270
	MLOC=MRB
	CPHERE=10
C
	DO 15 I=1,R2MAX
C						!CLEAR ROOM 2 ARRAY.
	  RROOM2(I)=0
	  OROOM2(I)=0
15	CONTINUE
C
	DO 20 I=1,XMAX
C						!CLEAR TRAVEL ARRAY.
	  TRAVEL(I)=0
20	CONTINUE
C
	DO 30 I=1,VMAX
C						!CLEAR VILLAINS ARRAYS.
	  VOPPS(I)=0
	  VPROB(I)=0
	  VILLNS(I)=0
	  VBEST(I)=0
	  VMELEE(I)=0
30	CONTINUE
C
	DO 40 I=1,OMAX
C						!CLEAR OBJECT ARRAYS.
	  ODESC1(I)=0
	  ODESC2(I)=0
	  ODESCO(I)=0
	  OREAD(I)=0
	  OACTIO(I)=0
	  OFLAG1(I)=0
	  OFLAG2(I)=0
	  OFVAL(I)=0
	  OTVAL(I)=0
	  OSIZE(I)=0
	  OCAPAC(I)=0
	  OCAN(I)=0
	  OADV(I)=0
	  OROOM(I)=0
40	CONTINUE
C
	RDESC2=0
C						!CLEAR DESC BASE PTR.
	DO 50 I=1,RMAX
C						!CLEAR ROOM ARRAYS.
	  RDESC1(I)=0
	  RACTIO(I)=0
	  RFLAG(I)=0
	  RVAL(I)=0
	  REXIT(I)=0
50	CONTINUE
C
	DO 60 I=1,MMAX
C						!CLEAR MESSAGE DIRECTORY.
	  RTEXT(I)=0
60	CONTINUE
C
	DO 70 I=1,AMAX
C						!CLEAR ADVENTURER'S ARRAYS.
	  AROOM(I)=0
	  ASCORE(I)=0
	  AVEHIC(I)=0
	  AOBJ(I)=0
	  AACTIO(I)=0
	  ASTREN(I)=0
	  AFLAG(I)=0
70	CONTINUE
C
	DBGFLG=0
	PRSFLG=0
C
C allow setting gdtflg true if user id matches wizard id
C this way, the wizard doesn't have to recompile to use gdt
C
C	Changed by TAA so that always in wizard ID
D	gdtflg=1
C
	FROMDR=0
C						!INIT SCOL GOODIES.
	SCOLRM=0
	SCOLAC=0
C INIT, PAGE 4
C
C NOW RESTORE FROM EXISTING INDEX FILE.
C
	OPEN(UNIT=1,file='/usr/share/games/dungeon/dindx.dat',
     &	status='OLD',FORM='FORMATTED',ACCESS='SEQUENTIAL',ERR=1900)
	READ(1,130) I,J,K
C						!GET VERSION.
	IF((I.NE.VMAJ).OR.(J.NE.VMIN))
     &	GO TO 1925

	OPEN(UNIT=DBCH,file='/usr/share/games/dungeon/dtext.dat',
     &	status='OLD',FORM='UNFORMATTED',ACCESS='DIRECT',
     &	recl=76,ERR=1950)

D	PRINT 150
D150	FORMAT(' RESTORING FROM "dindx.dat"')
	READ(1,130) MXSCOR,STRBIT,EGMXSC
	READ(1,130) RLNT,RDESC2,RDESC1,REXIT,RACTIO,RVAL,RFLAG
	READ(1,130) XLNT,TRAVEL
	READ(1,130) OLNT,ODESC1,ODESC2,ODESCO,OACTIO,OFLAG1,OFLAG2,
     &	OFVAL,OTVAL,OSIZE,OCAPAC,OROOM,OADV,OCAN,
     &	OREAD
	READ(1,130) R2LNT,OROOM2,RROOM2
	READ(1,130) CLNT,CTICK,CACTIO
	READ(1,135) CFLAG
	READ(1,130) VLNT,VILLNS,VPROB,VOPPS,VBEST,VMELEE
	READ(1,130) ALNT,AROOM,ASCORE,AVEHIC,AOBJ,AACTIO,ASTREN,AFLAG
	READ(1,130) MBASE,MLNT,RTEXT
C
	CLOSE(1)
	GO TO 1025
C						!INIT DONE.
C
C 130	FORMAT(I8)
130	FORMAT(I6)
135	FORMAT(L4)
C INIT, PAGE 5
C
C THE INTERNAL DATA BASE IS NOW ESTABLISHED.
C SET UP TO PLAY THE GAME.
C
1025	CALL INTIME(SHOUR,SMIN,SSEC)
C						!GET TIME AND DATE.
C	CALL IDATE(I,J,K)
C	CALL IDATE(DATARRY(1))
C	CALL INIRND(or(DATARRY(1),or(DATARRY(2),DATARRY(3))),
C     &	or(SHOUR,or(SMIN,SSEC)))
C	NEW WAY TO INITIALIZE /*TAA*/
	CALL INIRND(SHOUR*3600+SMIN*60+SSEC)
C
	WINNER=PLAYER
	LASTIT=AOBJ(PLAYER)
	HERE=AROOM(WINNER)
	THFPOS=OROOM(THIEF)
	BLOC=OROOM(BALLO)
	INIT=.TRUE.
C
D	PRINT 1050,RLNT,RMAX,XLNT,XMAX,OLNT,OMAX,MLNT,MMAX,
D    &  VLNT,VMAX,ALNT,AMAX,CLNT,CMAX,R2LNT,R2MAX
D1050	FORMAT(' USED:'/1X,I5,' OF',I5,' ROOMS'/
D    &  1X,I5,' OF',I5,' EXITS'/
D    &  1X,I5,' OF',I5,' OBJECTS'/
D    &  1X,I5,' OF',I5,' MESSAGES'/
D    &  1X,I5,' OF',I5,' VILLAINS'/
D    &  1X,I5,' OF',I5,' ADVENTURERS'/
D    &  1X,I5,' OF',I5,' CLOCK EVENTS'/
D    &  1X,I5,' OF',I5,' ROOM2 SLOTS')
D	PRINT 1150,MXSCOR,EGMXSC,RECNO,RDESC2,MBASE,STRBIT
D1150	FORMAT(' MAX SCORE=',I5/' EG SCORE=',I5/
D    &  ' MAX RECNO=',I5/' RDESC2 BASE=',I5/
D    &  ' MELEE START=',I5/' STAR MASK=',I7)
D	PAUSE 1
C
	RETURN
C INIT, PAGE 6
C
C ERRORS-- INIT FAILS.
C
1900	PRINT 910
	PRINT 980
	RETURN
1925	PRINT 920,I,J,K,VMAJ,VMIN,VEDIT
	PRINT 980
	RETURN
1950	PRINT 960
	PRINT 980
	RETURN
910	FORMAT(' I can''t open ','dindx.dat','.')
920	FORMAT(' "dindx.dat" is version ',I1,'.',I1,A1,'.'/
     &	' I require version ',I1,'.',I1,A1,'.')
960	FORMAT(' I can''t open ','dtext.dat','.')
980	FORMAT(' Suddenly a sinister, wraithlike figure appears before ',
     &'you,'/' seeming to float in the air.  In a low, sorrowful voice',
     &' he says,'/' "Alas, the very nature of the world has changed, ',
     &'and the dungeon'/' cannot be found.  All must now pass away."',
     &'  Raising his oaken staff'/' in farewell, he fades into the ',
     &'spreading darkness.  In his place'/' appears a tastefully ',
     &'lettered sign reading:'//23X,'INITIALIZATION FAILURE'//
     &' The darkness becomes all encompassing, and your vision fails.')
C
	END
C PROTCT-- CHECK FOR USER VIOLATION
C
C THIS ROUTINE SHOULD BE MODIFIED IF YOU WISH TO ADD SYSTEM
C DEPENDANT PROTECTION AGAINST ABUSE.
C
C AT THE MOMENT, PLAY IS PERMITTED UNDER ALL CIRCUMSTANCES.
C
	LOGICAL FUNCTION PROTCT(X)
	IMPLICIT INTEGER(A-Z)
C
	PROTCT=.TRUE.
	RETURN
	END

