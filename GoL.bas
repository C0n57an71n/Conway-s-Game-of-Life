' Conway's Game of Life (PicoCalc / MMBasic)


OPTION DEFAULT INTEGER
RANDOMIZE TIMER

'-----------------------------
' Config
'-----------------------------
CONST SCRW = MM.HRES
CONST SCRH = MM.VRES
CONST CELL = 10                 ' Try 10 or 8 for speed; 5 if you want more detail (slower)
CONST W = SCRW \ CELL
CONST H = SCRH \ CELL

' Colors
CONST wt = RGB(WHITE)
CONST bk = RGB(BLACK)

CONST INIT_FILL_PCT = 25
CONST STATUS_EVERY = 1

DIM a(W-1, H-1)     ' current
DIM b(W-1, H-1)     ' next
DIM gen, running, upd


running = 1
CLS bk

'-----------------------------
' Seed/Clear
'-----------------------------
SUB SeedRandom(pct%)
  LOCAL x, y
  FOR y = 0 TO H-1
    FOR x = 0 TO W-1
      a(x,y) = (RND * 100 < pct%)
    NEXT x
  NEXT y
END SUB

SUB ClearAll
  LOCAL x, y
  FOR y = 0 TO H-1
    FOR x = 0 TO W-1
      a(x,y) = 0
    NEXT x
  NEXT y
END SUB

'-----------------------------
' Draw
'-----------------------------
SUB DrawCell(x, y, alive)
  LOCAL x1, y1, c
  IF alive THEN c = wt ELSE c = bk
  x1 = x * CELL
  y1 = y * CELL
   BOX x1, y1, CELL, CELL, , c, c
END SUB

SUB DrawAll
  LOCAL x, y
  FOR y = 0 TO H-1
    FOR x = 0 TO W-1
      DrawCell x, y, a(x,y)
    NEXT x
  NEXT y
END SUB

SUB StatusBar
  ' redraw occasionally to avoid extra drawing overhead
  BOX 0, 0, 140, 12, , bk, bk
  TEXT 2, 2, "Gen:" + STR$(gen)
  IF running THEN
    TEXT 60, 2, "Run"
  ELSE
    TEXT 60, 2, "Pause"
  ENDIF
END SUB

'-----------------------------
' Init
'-----------------------------
SeedRandom INIT_FILL_PCT
DrawAll
gen = 0
upd = 0
StatusBar

'-----------------------------
' Main loop
'-----------------------------
DO
  ' input (non-blocking)
  k$ = INKEY$
  IF k$ <> "" THEN
    SELECT CASE UCASE$(k$)
      CASE " "         ' pause/resume
        running = 1 - running
        StatusBar
      CASE "R"         ' reseed
        SeedRandom INIT_FILL_PCT
        DrawAll
        gen = 0: upd = 0
        StatusBar
      CASE "C"         ' clear
        ClearAll
        DrawAll
        gen = 0: upd = 0
        StatusBar
      CASE CHR$(27)    ' Esc
        CLS bk
        END
        
    END SELECT
  ENDIF

  IF running THEN
    ' ----- compute next gen into b(), inline neighbor counting with wrap -----
    FOR y = 0 TO H-1
      ym = y - 1: IF ym < 0 THEN ym = H - 1
      yp = y + 1: IF yp = H THEN yp = 0
      FOR x = 0 TO W-1
        xm = x - 1: IF xm < 0 THEN xm = W - 1
        xp = x + 1: IF xp = W THEN xp = 0

        n = a(xm,ym) + a(x,ym) + a(xp,ym) + a(xm,y) + a(xp,y) + a(xm,yp) + a(x,yp) + a(xp,yp)

        IF a(x,y) THEN
          b(x,y) = (n = 2) OR (n = 3)
        ELSE
          b(x,y) = (n = 3)
        ENDIF
      NEXT x
    NEXT y

    ' ----- draw only the cells that changed -----
    FOR y = 0 TO H-1
      FOR x = 0 TO W-1
        IF b(x,y) <> a(x,y) THEN DrawCell x, y, b(x,y)
      NEXT x
    NEXT y

    ' ----- copy b -> a -----
    FOR y = 0 TO H-1
      FOR x = 0 TO W-1
        a(x,y) = b(x,y)
      NEXT x
    NEXT y

    gen = gen + 1
    upd = upd + 1
    IF upd >= STATUS_EVERY THEN
      upd = 0
      StatusBar
    ENDIF
  ENDIF

  ' No PAUSE for max speed; add "PAUSE 1" if you want to throttle
LOOP
