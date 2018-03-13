Requirements to tilemap

ONLY ONE SPACE INDENTS
NO EMPTY STRINGS
EDGES w** | a**
SIZE N*N

Designations
* = {0..9}
? = Whatever

NOTHING
  v?? Void
BOXES
{
  w** Wall -- ** - texture number
  a** Animated Wall -- ** - texture number
  d** Destr. wall -- ** - texture number
}
START POSITIONS
{
  p?? Player
  e** Enemy -- ** - enemy type
}
SPRITES
{
  i** Item -- ** - item type
  b** Background -- ** - texture number, b00 - level end
  o** Object -- ** - texture number
}
