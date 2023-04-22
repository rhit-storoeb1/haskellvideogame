# haskellvideogame: 

# Player Data
- Health: 100
- Movement: wasd
- Limited to moving on the screen

## Weapons
- Weapon 1: Machine Gun (h)
  - shoot small bullets forward in a line, lower damage per bullet
- Weapon 2: Shorter-range melee weapon that shoots to the side of the player (k)
  - medium damage but doesn't reach far
- Weapon 3: Placeable mine (j)
  - player places the mine, and it explodes after a few seconds, dealing high damage to enemies

# Scrolling Background

# Enemy Data

## Enemy 1
- Health: 50
- Weapon: weak bullets shot straight
- Movment Pattern: zig zag through the middle of the screen, shoots a bullet when it changes directions

## Enemy 2
- Health: 30
- Weapon: stronger bullets show straight
- Movement Pattern: starts from the left side of the screen and sweeps across the screen, accelerating downwards

## Enemy 3 - Boss
- Health: 500
- Weapon: Spray machine gun
- Movement Pattern: does a "sqare zig zag" across the screen, shooting regularly. 
- Other: 
  - When the boss is spawned, no other enemies spawn until the Boss is defeated
  - The boss has a low chance of spawning every time an enemy dies/leaves the screen
