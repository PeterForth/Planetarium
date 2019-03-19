\  PLANETARIUM  -- BY PETERFORTH --2018
\  SIMULATION OF THE SOLAR SYSTEM WITH ORBITS ACCORDING TO REAL DATA(TIME).
\   simplified simulation of the orbits of PLANETS + COMET
\    ANS Forth -- Win32forth.
\    MINIMUM GRAPHICS REQUIREMENTS : SCREENRESOLUTION AT 1600  X  858 PIXELS.

\  the purpose of this program is  :
\  to teach  how to create simple graphics and animations using the  console panel
\  on win32forth  -- this permits the programmer to concentrate on graphics words
\  only and not on complex windows initialization and handling. (as on DOS type Forths)
\  understand  the movement of the planets arround our star, the sun.
\  and awaken young programmers the love for astronomy observation, simulation on computers
\  and math , using a   simplified model  and  perspective projection
\  my contact for your comments, improvements, or support  -*-  peter4th2017@gmail.com
\  learn further win32forth programming -*-  https://sites.google.com/view/win32forth/

decimal


NEEDS SUPPORT-PLANETARIUM2.F
0 value trouble


   500  constant Xmax
   500  constant Ymax

f# 1.2 fconstant Xscale
F# 1.2 fconstant Yscale

 needs toolset.f

\ =========================================================================================
\ zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz


WinDC theDC                      \ =========================================

\   WinDC CurrentDC
    : graphics-in-console ( - ) \ initialize DC for the console
         GetHandle: cmd CALL GetDC PutHandle:   theDC                          \  currentDC
   ;


: SCALE         ( -- 'x 'y ) ( F: x y -- )
                Ymax 10 / 4 * s>f Yscale f/ f* f>d drop Ymax 2 / +
                Xmax 10 / 4 * s>f Xscale f/ f* f>d drop Xmax 2 / + swap
;


: graph-init    ( -- )
        \        Xmax Ymax 32 xcall drop                 \ set console pixel size
          \      CONDC               PutHandle: theDC    \ init DC to the console
      graphics-in-console

          WHITE            SetTextColor: theDC
                BLACK              SetBkColor: theDC
                cls
                cursor-off
             \   Xmax 40 /          YELLOW moon    attributes!
             \   Xmax 15 /            GREEN  earth     attributes!
             \     Xmax 15 /             YELLOW   sun        attributes!
;



\ %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\   GRaphics peter  file



: reversed      ( - )            16777215 1 fgbg! ;
: normal        ( - )            1 -1 fgbg! ;
  255 255 255 rgb      \ set the color buffer to "white"

ANEW graphics.f

vocabulary graphics

: gr \in-system-ok also
     \in-system-ok graphics ;

only forth also graphics definitions




\ 0 255 0 rgb newcolor: black \ \ ( turtle ) 32000 do test

0 0 0 palettergb new-color  current-color

WinDC CurrentDC

: graphics-in-console ( - ) \ initialize DC for the console
  GetHandle: cmd CALL GetDC PutHandle: currentDC
  ;

 \ (( Disable or delete this line for the following test.
: test
    graphics-in-console
    255 0 do
      255 0 do
        255 0 do
          i k j rgb newcolor: current-color
          i 0 10 i + 200  current-color FillArea: currentDC
       7 +loop
    2 +loop
   loop  ;



:inline pick-a-color       ( N -- color ) \ ( N -- rgb )
  dup 0<> if PosblColors /mod drop then PC_NOCOLLAPSE + ;

7 value colorstep
7 value rgb-to-inc

256 constant max#r#g#b
max#r#g#b cell-array #used-starting-color ( n - #used )

: +green ( colorref +green - colorref+green )     8 lshift +  ;
: +blue  ( colorref +blue  - colorref+blue )     16 lshift +  ;
synonym +red +

: random-flag ( - 1 | - -1 )
   2 random 1 =
     if    true
     else  false
     then
  ;

: -rot-drop ( n1 n2 n3 - n3 n2 )   -rot drop ;


: +rgb ( colorref +rgb - colorref+rgb )
   rgb-to-inc tb 1   and if 2dup +red   -rot-drop then
   rgb-to-inc tb 10  and if 2dup +green -rot-drop then
   rgb-to-inc tb 100 and if 2dup +blue  -rot-drop then drop    ;

: incr-used-color ( n - n )
   1 over +to-cell #used-starting-color ;

\ 0 max#r#g#b ' #used-starting-color cadump  abort

: select-a-startcolor (  max-#color - #color )
   dup random swap  ['] #used-starting-color
   least-used  incr-used-color
 ;

: _random_color ( - rgb )
    max#r#g#b select-a-startcolor
    max#r#g#b select-a-startcolor
    max#r#g#b select-a-startcolor rgb  ;

0 256 0 rgb variable color_false color_false !
12709967 variable color_true color_true !
0 value color_false_bck
0 value color_true_bck


: random-color ( - )
     today 2drop random 1 max 5 * to rgb-to-inc
\    3 random  to rgb-to-inc
   _random_color  _random_color 256 random to colorstep
          random-flag
              if    color_false ! color_true !
              else  swap color_true ! color_false !
              then
 ;

0 value color
defer way-to-color
' color is way-to-color
 0 0 255  rgb to color

create &InfoRect  4 cells allot    ( - &InfoRect )
&InfoRect 4 cells erase
&InfoRect constant window_x
&InfoRect 1 cells+ constant window_y
&InfoRect 2 cells+ constant width
&InfoRect 3 cells+ constant height

: getwindow ( - hwnd x y w h )
   &InfoRect Call GetActiveWindow dup
   -rot Call GetClientRect ( GetWindowRect ) ?win-error
   window_x @ window_y @ width @ height @
 ;

\  getwindow tp abort


: GetClientRect_window  ( - )
   &InfoRect Call GetActiveWindow Call GetClientRect drop
 ;

: Xmax    ( - width )    GetClientRect_window width @ ;
: Ymax    ( - Ymax  )    GetClientRect_window height @ ;

-1 value prev-y  -1 value  prev-x
0.1e fvalue distance

\ 67108864 constant rgb-offset
16777216 constant PosblColors

: moveto        ( x y -- )
   2dup Moveto: CurrentDC  to prev-y  to prev-x pause ;

: lineto        ( x y -- )
\    way-to-color newcolor: current-color
\    current-color LineColor: CurrentDC
     way-to-color pick-a-color gethandle: CurrentDC Call SetTextColor drop
    2dup LineTo: CurrentDC to prev-y  to prev-x ;


: pixel-on      ( x y -- )
   2dup to prev-y  to prev-x swap
   way-to-color @ -rot
   GetHandle: CurrentDC Call SetPixelV drop pause
 ;

 ' color is way-to-color \ defer the color when using set-dot

defer set-dot

: dot>screen ( x y rgb -- )
   -rot swap GetHandle: CurrentDC Call SetPixelV drop ;

' dot>screen is set-dot
( magenta cyan brown ltgray gray red ltred ltblue ltgreen dkgray dkmagenta  )
: p    ( x y --)   black   ( green rgb  )  set-dot ;
: p2    ( x y --)   green   ( green   rgb  )  set-dot ;
: p3    ( x y --)   blue   ( yellow  rgb  )  set-dot ;
: p4    ( x y --)   white   (  lila rgb  )  set-dot ;
: p5    ( x y --)   cyan  (  red   rgb  )  set-dot ;
: p6    ( x y --)   red   (  lila rgb  )  set-dot ;
: p7    ( x y --)   magenta   (  lila rgb  )  set-dot ;
: p8    ( x y --)   brown     (  white  rgb  )  set-dot ;

:  bb  300 0 do  i  150 p  loop ;


synonym set-mdot       set-dot

: ptest
   cls graphics-in-console
   800 0 do i 37 0 0 255 rgb set-dot loop
   650 0 do i 31 0 255 0 rgb set-dot loop
   650 0 do i 32 255 0 0  rgb set-dot loop
   650 0 do i 33 0 0 0 rgb set-dot loop
  ;



0e FVALUE win.xleft
0e FVALUE win.xright
0e FVALUE win.ybot
0e FVALUE win.ytop
0e FVALUE win.xdif
0e FVALUE win.ydif


variable SXoffs
variable SXdiff
variable SYoffs
variable SYdiff

1.0e FVALUE PenX
1.0e FVALUE PenY


: SET-GWINDOW   \ <xb> <yb> <xt> <yt> --- <>  F: <xb> <yb> <xt> <yt> --- <>
                2OVER  SYoffs ! SXoffs !
                ROT  - SYdiff !                 \ hardware coordinates!
                SWAP - SXdiff !
                FTO win.ytop
                FTO win.xright
                FTO win.ybot
                FTO win.xleft
                win.xright win.xleft F- FTO win.xdif
                win.ytop  win.ybot   F- FTO win.ydif ;

: SCALE         \ F: <x> <y> --- <>  <> --- <x> <y>
                win.ybot  F-  win.ydif F/  SYdiff @ S>F F*  F>S SYoffs @ +
                win.xleft F-  win.xdif F/  SXdiff @ S>F F*  F>S SXoffs @ +
                SWAP ;

\ -- Won't plot a point that doesn't fall within the window.
: PLOT-POINT    PenX win.xleft win.xright       \ <color> --- <>
                F2DUP F> IF FSWAP ENDIF
                  FWITHIN 0= IF DROP EXIT ENDIF
                PenY win.ybot  win.ytop
                F2DUP F> IF FSWAP ENDIF
                  FWITHIN 0= IF DROP EXIT ENDIF
                PenX PenY SCALE ROT SET-DOT ;

: xypos        ( x y -- )
                 to prev-y  to prev-x ;

synonym plot lineto     synonym xyplot moveto

: -line          ( _x2 _y2 _x1 _y1 -- )
                2swap moveto lineto ;

: line          ( _x2 _y2 _x1 _y1 -- )
                2swap moveto lineto ;

: draw-mline    \ <xb> <yb> <xe> <ye> <color> --- <>
                ( to color way-to-color ) LineColor: CurrentDC line ;

create sinus
     0 ,  1745 ,  3490 ,  5234 ,  6976 ,  8716 , 10453 , 12187 , 13917 ,
 15643 , 17365 , 19081 , 20791 , 22495 , 24192 , 25882 , 27564 , 29237 ,
 30902 , 32567 , 34202 , 35837 , 37461 , 39073 , 40674 , 42262 , 43837 ,
 45399 , 46947 , 48481 , 50000 , 51504 , 52992 , 54464 , 55919 , 57358 ,
 58779 , 60182 , 61566 , 62932 , 64279 , 65606 , 66913 , 68200 , 69466 ,
 70711 , 71934 , 73135 , 74314 , 75471 , 76604 , 77715 , 78801 , 79864 ,
 80902 , 81915 , 82904 , 83867 , 84805 , 85717 , 86603 , 87462 , 88295 ,
 89101 , 89879 , 90631 , 91355 , 92050 , 92718 , 93358 , 93969 , 94552 ,
 95106 , 95630 , 96126 , 96593 , 97030 , 97437 , 97815 , 98163 , 98481 ,
 98769 , 99027 , 99255 , 99452 , 99619 , 99756 , 99863 , 99939 , 99985 ,
100000 ,

: (sinus)  4 * sinus + @ ;   ( angle - unsigned_sin*100000 )

: sin  ( angle - sin*100000 )
   dup abs dup
   360 > if  360 mod
         then dup
         91 < if (sinus)                                 \ < 90
              else dup 181 <
                   if 180 - abs (sinus)                   \ 91 - 179
                   else dup 271 <
                        if 180 - (sinus) negate            \ 180 - 269
                        else 360 - abs (sinus) negate      \ 270 - 360
                        then
                    then
                then
   swap 0< if negate
           then
 ;
\ 90 sin .s abort
: cos          ( angle - cos*100000 )
   90 - dup 0> >r
   abs sin r>
       if negate then
 ;





\ sinustest
800 value fromm 1 value scal1 1 value scal2
:  bbsinr   fromm  0 do  i  scal1 scal2  */   i sin 1000 /  300 + p5  loop ;  \  red
:  bbsinw  fromm  0 do  i  scal1 scal2  */    i sin 1000 /  300 + p3  loop ;  \ yellow white
:  bbsiny  fromm  0 do  i   i sin 1000 /  300 + p6  loop ;  \ white

: testsin  800 to fromm  20 0 do bbsinr  300 ms  bbsinw  100 ms loop  ;

:  circe   360  0 ?do   i cos 1000 /  200 +       i sin 1000 /  200 + p5  loop ;  \  red

:  circe2   360  0 do       i sin 1000 /  300 +
                                        i cos 1000 /  300 +
                2dup  p5   5 ms  p3    loop ;  \  red


\ *******************************************************************************************







\ MAXIMIZE CONSOLE
\  SW_MAXIMIZE hWnd Call ShowWindow Drop
\  SW_MAXIMIZE ConsoleWindow Call ShowWindow Drop
\   conHndl     HANDLE CONSOLE
\   mark_all

: WINDOW-MAXIMIZE   SW_MAXIMIZE conHndl  Call ShowWindow Drop  ;    \ CONSOLE
\ ==============================================================
WINDOW-MAXIMIZE
\ mark-all


\ %%%%%%%%%%%%%%%%%%%%%HOWTO TYPE TEXT IN GRAPHICS MODE%%%%%%%%%%%%%
0 value vscroll

: wcr ( x1 y1 - x2 y2 )      vscroll 7 + +  ;

: wtype  ( x y adr n - x y ) 2over 2swap textout: currentDC  ;

 cls graphics-in-console 700 5 s"  GRAPHICS MODE ON " wtype 2drop

\ %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



0 VALUE  OLDXX  0 VALUE  OLDYY

: >OLDPOS   ( N N --)   TO OLDYY  TO OLDXX  ;
: OLDPOS@  ( --N N )   OLDXX  OLDYY ;

( magenta cyan brown ltgray gray red ltred ltblue ltgreen dkgray dkmagenta  )

: (erase-planet)   (  n n n --)         BLACK      BrushColor: theDC      BLACK       LineColor: theDC
                                         ( oldvenus 2@  venusdiam )  FillCircle: theDC      ;


\  : (erase-planet)   (  n n n --)         BLACK      BrushColor: theDC      BLACK       LineColor: theDC
 \                           2 PICK 2 PICK  2>R  FillCircle: theDC 2R>  ( P5 )   cyan     set-dot   ;


defer erase-planet              \  '  noop is erase planet

: showorbit    ['] noop   is erase-planet     ;
: noshoworbit    ['] (erase-planet)  is  erase-planet     ;      noshoworbit    \ default noshoworbit



5 value mercurydiam
2variable oldmercury  0 0 oldmercury 2!

:   themercury   (  N N --)        \  2DUP  OLDvenus 2@ D= IF   2DROP EXIT THEN
                       oldmercury 2@  mercurydiam  erase-planet     \      ( erase at old position)
                              brown  BrushColor: theDC              \      brown   LineColor: theDC
                   2DUP  oldmercury 2!   mercurydiam  FillCircle:  theDC    ;





8 value venusdiam
2variable oldvenus  0 0 oldvenus 2!

:   thevenus   (  N N --)        \  2DUP  OLDvenus 2@ D= IF   2DROP EXIT THEN
                                   oldvenus 2@  venusdiam  erase-planet     \        ( erase at old position)
                                                  brown  BrushColor: theDC  \      brown   LineColor: theDC
                                    2DUP  oldvenus 2!        venusdiam   FillCircle:  theDC    ;


17 value saturndiam
2variable oldsaturn  0 0 oldsaturn 2!
:   thesaturn (  N N --)  2DUP  oldsaturn 2@ D= IF   2DROP EXIT THEN

                                   oldsaturn  2@   saturndiam erase-planet  \       ( erase at old position)
                                                dkmagenta  BrushColor: theDC        \  BLUE     LineColor: theDC
                            2DUP  oldsaturn  2!         saturndiam  FillCircle:  theDC    ;



18 value jupiterdiam
2variable oldjupiter  0 0 oldjupiter 2!
:   thejupiter  (  N N --)  2DUP  OLDJUPITER 2@ D= IF   2DROP EXIT THEN

                                   oldjupiter  2@   jupiterdiam  erase-planet         ( erase at old position)
                                                 BLUE   BrushColor: theDC        \  BLUE     LineColor: theDC
                            2DUP  oldjupiter  2!         jupiterdiam  FillCircle:  theDC    ;

8 value marsdiam
2variable oldmars  0 0 oldmars 2!
:   themars   (  N N --)   2DUP  OLDMARS 2@ D= IF   2DROP EXIT THEN

                                  oldmars 2@  marsdiam erase-planet       ( erase at old position)
                                              red   BrushColor: theDC      \    red     LineColor: theDC
                            2DUP  oldmars 2!        marsdiam   FillCircle:  theDC    ;

10 value earthdiam
2variable oldearth  0 0 oldearth 2!
:   theearth    (  N N --)
                                        oldearth 2@  earthdiam     erase-planet          ( erase at old position)
                                                   cyan  BrushColor: theDC          cyan     LineColor: theDC
                            2DUP  oldearth 2!     earthdiam     FillCircle:  theDC    ;


600   VALUE SUNPX 400 VALUE SUNPY  \ origin spot of the sun on screen

: THESUN    YELLOW     LineColor: theDC   YELLOW   BrushColor: theDC
                  SUNPX SUNPY  45    FillCircle:  theDC  ;

870 value distmercury
700 value distvenus
600 value distearth
500 value distmars
400 value distjupiter
300 value distsaturn

distmars value distcomet


white black  fgbg!


2 VALUE  IPMARS    11 VALUE IPJUPITER   0 VALUE IPvenus    29 VALUE IPsaturn
0 value II     0 value ipmercury

0 to screendelay  \  is normally set at 18 in forth !!
2 value delay       \  delay on screen for planets best  representation



: 4-  4 - ;

DEFER .DAYS&YEARS
: [.DAYS&YEARS] 0 1 AT-XY ." Days = "  II .  0 2 AT-XY ." Years = " II 360 / .  ;

'  [.DAYS&YEARS]  IS  .DAYS&YEARS
\  '  NOOP   IS  .DAYS&YEARS



:  uuu1 1800 random ;         : ppp1 1800 random ;

: stardots   ( graphics-in-console ) 4000  0 Do  uuu1 ppp1  p  loop  ;

graph-init    \  <<<<<----- WITHOUT THIS, IT WILL NOT SHOW ANYTHING.... sin esto no anda !!


600 15 s"  GRAPHICS MODE ON " wtype 2drop



\  : cols          ( -- n1 )               \ current screen columns
\  : rows          ( -- n1 )               \ current screen rows
\ CR  .( ROWS = )  ROWS .  \ CR  .( COLS = )  COLS .   CR
\  SUN CENTER 600   VALUE SUNPX 400 VALUE SUNPY  \ origin spot of the sun on screen
: centerscr      xmax  2/   s>f  ymax  2/  s>f  ;

700 TO SUNPX
160 VALUE RM0   \ RIGHT MARGIN

: INFO-PLANETS ( --)
\ Mars   24.077 km/s,
RM0 5 AT-XY ." * PLANETS IN OUR SOLAR SYSTEM *"
RM0 6 AT-XY ." Mercury:  87.97 days  (0.2 years)"
RM0 7 AT-XY ." Venus : 224.70 days   (0.6 years)"
RM0 8 AT-XY ." Earth: 365.26 days    (1.0 year)"
RM0 9 AT-XY ." Mars: 686.98 days     (1.9 years)"
RM0 10 AT-XY ." Jupiter:4,332.82days (11.9 years)"
RM0 11 AT-XY ." Saturn:10,755.70 days(29.5 years)"
RM0 12 AT-XY ." Uranus: 30,687.15days(84 years)"
RM0 13 AT-XY ." Neptune:60,190.03days(164.8 years)"  ;

\ *****************
\ *****************
:  CAPTURE-KEY    ( key  )    ;

\ ROTATION MATRIX
\ x' = x cos f - y sin f     \ y' = y cos f + x sin f

0e FVALUE 'X  0e FVALUE 'Y     78e FVALUE ran  \ ROTATION ANGLE

: ROTATEXY  ( N N -- 'N 'N )        S>F TO 'Y   S>F  TO 'X
               ran FCOS 'X F*     ran FSIN  'Y F*   F-   F>S      \  x' = x cos f     -   y sin f
               ran FCOS 'Y F*     ran FSIN  'X F*   F+   F>S      \  y' = y cos f     +   x sin f
;    \  1e +fto   ran ;



\ *****************
\ *****************


6 to delay

:  PLANETARIUM    ( --)  ( 1 TO DELAY  )     stardots
                                                   THESUN  thesun info-planets

                            0 1  AT-XY  20 SPACES      \  showorbit    ---    noshoworbit
        0 TO II
                  BEGIN  1 +TO  II       .DAYS&YEARS              \  ON TEXT LEFT TOP CORNER
                                             (  II sin distearth  /  sunpx +  II cos  distearth  /  sunpy + theearth )
                               II sin     4 *  2/    distearth  /  sunpx +
                               II cos    distearth              /  sunpy +    theearth


    II 4- sin    4 *  2/ distearth  /  sunpx +
    II 4- cos              distearth / sunpy +         p4 ( P5 = cyan set-dot )




\ %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% comet  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
((
        II sin     4 *  2/    distearth  /  sunpx +
                               II cos    distearth              /  sunpy +    theearth
))
 \  II 4- sin    ( SIN + )  4 *  2/ distearth  /  sunpx +      II 4- cos    (  COS + )         distearth / sunpy +     ROTATEXY


 II 4- sin    ( SIN + )  4 *  2/ \
 II 4- cos    (  COS + )     ROTATEXY   distcomet  / sunpy + 50 + >r   distcomet  /  sunpx +  r>
  p4



\  #############################     MERCURY  ####################################

   II  s>f   2e-1  f/  f>s to  ipmercury
                               ipmercury   sin 4 *  2/  distmercury /  sunpx +    iPmercury       cos    distmercury /  sunpy +  themercury
ipmercury 4-  sin 4 *  2/  distmercury /  sunpx +        ipmercury  4-     cos    distmercury /  sunpy +  p2

\ themercury



\ ###############################   VENUS ########################################
   II  s>f   6e-1  f/  f>s to  ipvenus
                  iPvenus   sin 4 *  2/  distvenus /  sunpx +         iPvenus       cos    distvenus /  sunpy +  thevenus
  iPvenus 4-  sin 4 *  2/  distvenus /  sunpx +         iPvenus  4-     cos    distvenus /  sunpy +  p2



\  #############################     MARS  ####################################
   II  s>f  1.9e0  f/  f>s to  ipmars
                   iPMARS   sin 4 *  2/  distMARS /  sunpx +        iPMARS     cos    distmars /  sunpy +  themars

 iPmars 4-  sin 4 *  2/  distmars /  sunpx +         iPmars  4-     cos    distmars /  sunpy +  p2

\  #############################    JUPITER  ####################################
  II  s>f  11.9e0  f/ f>s  to  ipjupiter
                                                     iPJUPITER sin 4 *  2/  distjupiter / sunpx +       iPJUPITER  cos   distjupiter /  sunpy +  thejupiter

  iPJUPITER 4- sin 4 *  2/  distjupiter / sunpx +       iPJUPITER 4- cos   distjupiter /  sunpy +  P7



\  #############################     SATURN  ####################################

  II  s>f  29.5e0  f/ f>s  to  IPsaturn
                                                     IPsaturn   sin 4 *  2/   distsaturn / sunpx +       IPsaturn  cos   distsaturn /  sunpy +  thesaturn


    IPsaturn 4-  sin 4 *  2/   distsaturn / sunpx +       IPsaturn 4- cos   distsaturn /  sunpy +  p6





   key? if    CAPTURE-KEY 1 ELSE   delay ms 0 then

  UNTIL  CR  ."  STOP " CR ;          \  red


((
theta = 0;  // angle that will be increased each loop
h = 12      // x coordinate of circle center                         k = 10      // y coordinate of circle center
step = 15;  // amount to add to theta each time (degrees)

repeat until theta >= 360;
    { x = h + r*cos(theta)
      y = k + r*sin(theta)
      draw a line to x,y
      add step to theta
    }
))
  ((   ellipse :      var x = h +       r*Math.cos(theta) ;
                      var y = k - 0.5 * r*Math.sin(theta) ;   ))


:  ellipse2   360  0 do    i sin      1000 /  300 +   40 +
                           i cos 2 /  1000 /  300 +  20 +
                             2dup  p5   5 ms
                             p3    loop ;  \  red


: stop-key ( - )         key?           if   key abort  then    ;

: pauze     ( -- )    \ wait for a key <esc> will abort
     begin
       key?
     until key 27 =
   if cr ." Stoped !" abort then
 ;


: 0plot         ( -- )           0 0 xyplot    ;

0 value angle
: clear ( - ) \ puts 0 in the turtle
   0plot  0 to angle
 ;

: center
   width @ 2 / height @ 2 / xyplot 0 to angle
 ;

: turn          ( angle - )
   angle + dup 360 >=
    360 and -
    dup -360 <=
    360 and +
    to angle
 ;

: /0 ( n1 q - / )      \ avoids divide by zero
  dup 0=
        if 2drop 0 exit
        then /
 ;

: */rounded ( n1 n2 q - */rounded ) \ gives a better result
    dup 0=
        if drop 2drop 0 exit
        then
    dup >r */mod swap 10 * r> / 5 >= abs +
 ;

: (pencil        ( length -)
    dup angle cos 100000 */rounded
    prev-x + swap angle sin 100000 */rounded  prev-y + lineto
 ;

: pencil ( length -)
    way-to-color LineColor: CurrentDC
    (pencil
 ;

: slowpencil ( lengte slow - )
    way-to-color LineColor: CurrentDC
     swap 0
     do
    \ dup 0 do loop
     1 (pencil
    loop drop
 ;

: penup  ( length - )
   dup angle cos 100000 */rounded prev-x  + swap
     angle sin 100000 */rounded
     prev-x + to prev-y to prev-x
 ;




: InformationBox   { adr len \ message$ -- }
     MAXSTRING localAlloc: message$
      adr len message$ place
      message$ +NULL
      MB_OK  MB_ICONINFORMATION  or
      MB_TASKMODAL or
      z" Information:"
      message$ 1+
      NULL call MessageBox drop
 ;



\ %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
: .graph  noop ;
defer display
: orbit noop ;

: graph-orbit        ( -- )         0 to trouble
        ['] .graph IS DISPLAY
        graph-init
\ white black  fgbg!

        orbit
  \   cr cr ." I make it   stop on the day 22   see   ' 21 > ', because if not it will crash"
      \  graph-exit    ;
;


: simpleorbit        ( -- )         \ does nothing in this version of planetarium
     \   ['] .graph IS DISPLAY
                                    \ graph-init        \     \ white black  fgbg!
        orbit   ;

   \ graph-init

\ : orbit    ( --)   graph-orbit      ;

((
cr
.(  --->  DO     orbit  , to see the moon and the Earth orbits )   cr
.(  --->  DO     circe  or circe2   , to see a circular orbiting demo ) cr
))

\ : centerscr      xmax  2/   s>f  ymax  2/  s>f  ;
\ : testmoon     centerscr  moon  .object   ;  \ does nothing in this version of planetarium




planetarium









