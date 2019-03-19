

1.520e11  fconstant sun2earth   \ meters
3.844e8   fconstant earth2moon
5.979e24  fconstant earthmass   \ kg
1.991e30  fconstant sunmass
7.35e22   fconstant moonmass

3.0e4     fconstant earthv0     \ starting velocity
1.0e3     fconstant moonv0

1.0e3     fconstant stepsize    \ time between snapshots in seconds
0.0e0     fconstant fzero
6.67e-11  fconstant grav        \ gravitational constant



\S

This file is  for future upgrades and test-code

\ I will not use any gravitation calculus for simple planetarium level 1


0 value trouble

   500  constant Xmax
   500  constant Ymax

f# 1.2 fconstant Xscale
F# 1.2 fconstant Yscale



decimal


false value singlestep?         \ used to display slowly

: body          ( -- ) ( -- a )
\ create body parameters
\ structure: positionXY, velocityXY, mass ( 5 floats )

\       old-posXY, radius, color ( 4 ints )
\       assume float-aligned == cell aligned!
        create 5 floats allot  4 cells allot ;


: 'position     ( 'body -- 'position )      ;
: 'velocity     ( 'body -- 'velocity )      2 floats + ;
: 'mass         ( 'body -- 'mass )          4 floats + ;
: 'oldpos        ( 'body -- 'oldpos )        5 floats + ;
: 'radius        ( 'body -- 'radius )        [ 5 floats 2 cells + ] literal + ;
: 'color        ( 'body -- 'color )        [ 5 floats 3 cells + ] literal + ;


2variable eons                \ how old is this universe (in seconds) ?
\ 20000 value displaystep \ seconds between simulation print-outs

8000 value displaystep
variable displaycount

: displayable?  ( -- f )
\ return T every displaystep-th second (20000 => 5 hours)
        eons 2@         stepsize f>d d+  2dup eons 2!
        drop displaystep mod 0= ;

0 value trouble

: .eons                ( -- )
\ display the time in years, days, hours, minutes and seconds
        eons 2@ ( d)        \ seconds since start of universe
        [ 60 60 *  24 *         365 * ] literal um/mod 4 .r ."  years, "

       [ 60 60 *  24 * ]         literal   /mod 3      .r ."  days, "

      \ [ 60 60 *  24 * ]         literal   /mod 3      1  pick
                                 \  21 > if ( 1 to trouble   abort)  then      \ ************ STOP !!
            \                                .r ."  days, "

                   \  ." wait "  key
 \ 30  ms       \  ******************  here  a small delay ***************

        [ 60 60 *  ]                 literal   /mod 2 .r ."  hours, "
        60 /mod 2 .r ."  minutes and " 2 .r ."  seconds." ;


\ B/FLOAT
\ b/float offset FLOAT+        ( addr1 -- addr1+8 )

\ : FLOATS        ( n1 -- n1*8 )
\                B/FLOAT * ;

\ ================= VECTOR ARITHMETIC ======================

: v@            ( 'v -- fx fy )
\ fetch the x,y components of vector
        dup >r f@
        r> 1 floats + f@ ;

: v!            ( fx fy 'v -- )
\ store the x,y components of vector
        dup >r 1 floats + f!
        r> f! ;

: vnegate       ( va -- -va )
        fswap fnegate fswap fnegate ;

: v+            ( va vb -- vc )
\ add two vectors
        frot f+                 ( ax bx cy )
        fswap frot f+           ( cy cx )
        fswap ;

: v-            ( va vb -- va-vb )
\ subtract two vectors
        vnegate v+ ;

: vdup          ( v -- v v )
        fover fover ;

: fsqr          ( f -- f^2 )
        fdup f* ;

: distance      ( x y -- dist )
        fsqr fswap fsqr f+ fsqrt ;

: v*            ( va scalar -- vb )
        fdup frot f*            ( x s sy )
        frot frot f*            ( sy sx )
        fswap ;

: v/            ( va scalar -- vb )
        1.0e0 fswap f/ v* ;


\ ================= MOTION CALCULATIONS ======================

fvariable massmass

: getmasses     ( 'body1 'body2 -- )
\ get the mass of each body and multiply them together
        >r 'mass f@
        r> 'mass f@
        f* massmass f! ;

: attraction    ( 'body1 'body2 -- fx fy )
\ calculate attraction between body1 and body2
\ using mass and inverse-square law: f = G*m1*m2/r^2
        2dup getmasses
        swap >r v@  r> v@   v-  \ vec1-vec2
        vdup distance           ( x y r )
        massmass f@  grav f*    ( x y r mmg )
        fover f/ fover f/
        fswap f/                ( x y force/r )
        v* ;                    \ square it ( z/r * 1/r/r )

: increment     ( vdelta 'va -- )
\ add a differential amount to a vector
        stepsize v*             \ delta = delta * stepsize
        dup >r v@ v+  r> v! ;   \ v = v + delta

: motion        ( force 'body -- )
\ update the motion parameters of a body given an acting force
        >r
        r@ 'mass f@ v/          \ a = f/m
        r@ 'velocity increment  \ update velocity
        r@ 'velocity v@
        r> 'position increment  \ update position
        ;

: startup       ( mass position velocity 'body -- )
\ initial parameters, scalar quantities.
\ position is on the X axis, velocity is in the Y direction.
        >r
        fzero fswap r@ 'velocity v!
        fzero       r@ 'position v!
        r> 'mass f! ;

body earth
body sun
body moon

: .XY           ( x y scale -- )
\ print normalized XY coordinates
        v/ fswap f. f. ;

: .moonXY       ( -- )
\ position of moon relative to earth
        moon v@  earth v@  v-
        ." MoonXY: " earth2moon .xy ;

: .earthXY      ( -- )
\ position of earth relative to sun
        earth v@  sun v@  v-
        ." EarthXY:" sun2earth .xy ;

DEFER DISPLAY ( -- )

: .text ( -- )
        0 0 AT-XY .eons
        cr .earthxy .moonxy ; ' .text IS DISPLAY



: orbit
\ run the simulation
         0. eons 2!                        \ Let there be Light...

        earthmass                       \ set earth in motion
        sun2earth
        earthv0
        earth startup

        moonmass                        \ set moon in motion
        sun2earth earth2moon f+
        moonv0 earthv0 f+
        moon startup
\ zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz
        sunmass                         \ sun is stationary

        fzero fzero
        sun startup       \ 300.E0   300.E0
\ zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz
        begin   displayable?
              if        DISPLAY
                        key? singlestep? or
                        if        key   27 <> \ check for ESC key
                        else        true
                        then
              else        true
              then

             trouble not and
        while        earth moon attraction               \ earth moon attraction
                earth sun attraction v+
                earth motion            \ move the earth
                moon earth attraction
                moon sun attraction v+
                moon motion             \ move the moon
        repeat   ;

: goslow        ( -- )
        true to singlestep? ;

: text-init        ( -- )
        page ;

: text-exit        ( -- )
        ;

: text-orbit        ( -- )
        ['] .text IS DISPLAY
        text-init
        orbit
        text-exit ;

\ ========================== GRAPHICS =========================

: attributes!        ( radius color 'body -- )
\ initial parameters for the object.
        >r
        r@ 'color !
        r@ 'radius !
        0. r> 'oldpos 2! ;


   500  constant Xmax
   500  constant Ymax

f# 1.2 fconstant Xscale
F# 1.2 fconstant Yscale

\       : attributes!        ( radius color 'body -- )
\        initial parameters for the object.



: graph-exit    ( -- )
                cursor-on
 ;

: .OBJECT        ( 'body -- ) ( F: x y -- )
\ Move an object from its old position to (x,y)
        >r
        BLACK                        LineColor: theDC
        BLACK                       BrushColor: theDC
        r@ 'oldpos 2@  r@ 'radius @ FillCircle: theDC ( erase at old position)
        r@ 'color  @                 LineColor: theDC
        r@ 'color  @                BrushColor: theDC

        SCALE 2dup  r@ 'oldpos 2!       ( save the new object position)
        r> 'radius @              (  Fill )  Circle:  theDC  ;  \ ********************  FILL CIRCLE !!!!!! *******
                                         \  FILLCIRCLE

: moon-|-earth        ( -- )
\ position of moon relative to earth
        moon v@         earth v@  v-  earth2moon v/ moon .OBJECT ;

: earth-|-sun        ( -- )

\ position of earth relative to sun
        earth v@  sun v@  v-  sun2earth v/ earth .OBJECT ;

: .graph1        ( -- )
        0 0 AT-XY .eons
        moon-|-earth
        earth-|-sun ;

: .graph        ( -- )   \ 3000 ms
        0 0 AT-XY .eons     earth-|-sun
        moon-|-earth
         ;


