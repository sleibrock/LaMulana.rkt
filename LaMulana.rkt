#lang racket/base

#|
The La-Mulana Path Analyzer Program

La-Mulana is an archaeological exploration platforming game
dating back to 2006 as a fun retro freeware game released
for free.

In 2011, the game was re-released by it's developing company
Nigoro and released across several platforms slowly -
starting with the Wii Virtual Console (VC) platform.

La-Mulana is now a hit game across multiple platforms including:
* PC / macOS / Linux
* Wii VC
* PlayStation Portable Vita (PSVita)
* PlayStation 4
* Xbox One
* Nintendo Switch

The gameplay of La-Mulana is heavily based around exploration,
item collection, puzzle solving and combat. La-Mulana is a game
heavily designed for speedrunning. A normal, "first" playthrough
of La-Mulana could take you anywhere between 30-80 hours depending
on how lost you get. The speedrun is less than two hours.

Optimizing information in the game and remembering pathways is
vital to La-Mulana, and makes it very fun. There is a slight
amount of RNG in the game based on enemy drops or boss state machine
move cycles, but once you know the game, it's really fun to play
and try to get a lower time score.

The ending credits is where your total time is revealed, though
you can keep track of your progress in the pause menu.

This program is designed to reveal the most optimal path for
collecting items to beat the game.

Items are treated like a graph data structure, and collecting
a certain amount of items will let you eventually beat the game.

But which items do you need to beat? And do items require other
items to unlock? Yes, yes they do.

This program will attempt to:
* Tag all items in the game with their requirements
* Create a graph data structure representing all items
* Create a tree walk simulation program to emulate gameplay
* Analyze game paths and see what the most "optimal" order is


Gameplay Guide for those reading:
* La-Mulana is broken up into 19 different levels
* There are 9 rooms, each with two different sides - front and back
* There is at minimum one boss per each room
* The final boss requires all 8 main boss guardians to be defeated
* Certain items are needed to access each boss

Speedrun guide:
* Beat Boss 1 through 4, collect as many coins/items as possible
* Start working on backside rooms to reach Boss 5
* Collect more items, puzzles, and then gradually defeat Boss 6 and 7
* Collect another batch of important items, then defeat Boss 8
* Finally, collect all items and activate all puzzle pieces
* Defeat Boss 9, and then escape La-Mulana
|#


(require (only-in racket/cmdline command-line)
         (only-in racket/contract -> any/c or/c hash/c listof define/contract parameter/c)
         )


; a toggle to turn on 100% collectathon or not
(define/contract *collect-everything?*
  (parameter/c boolean?)
  (make-parameter #f))


(define/contract (only-if-wrap P)
  (-> (parameter/c boolean?) (-> any/c any/c))
  (λ (value) (if (P) value '())))

(define/contract only-if-100%
  (-> list? list?)
  (only-if-wrap *collect-everything?*))
  
; This is the complete and total graph of items in La-Mulana
; All NECESSARY items are here
; Unnecessary items are tracked, but are not required for end-of-game
; A setting can be toggled on to include them as needed
(define/contract (Build-Graph)
  (-> (hash/c symbol? (listof symbol?)))
  (make-immutable-hash
   `(; items in the surface
     (scanner     . ()) ; store 10g
     (shell       . ()) ; free
     (feather     . (serpent)) ; argus
     (birth       . (origin)) ; cliffside waterfall
     (lifeup1     . ()) ; below the seal
     (map1        . ()) ; on a skeleton
     (buckler     . ()) ; store
     (waterproof  . ()) ; store
     (laptop2     . '(jewel1 jewel2 jewel3 jewel4)) ; after 4 guardians
     (mulana      . (diary)) ; after diary sequence
     (reader.exe  . ()) ; buy from store 50g
     (deathv.exe  . ()) ; free behind xelpud statue
     (yagomap.exe . ()) ; store 20g
     (mekuri.exe  . ()) ; waterfall ladder secret tent

     ; items in gate of guidance
     (grail      . ()) ; holy grail is required later on
     (shuriken   . ()) ; screen transition glitch
     (jewel1     . ()) ; amphisbaena's jewel
     (lifeup2    . ()) ; entrance
     (map2       . ()) ; chest before elevator
     (crucifix   . (flare life)) ; anti-ghosts
     (treasure   . (pepper)) ; gate of illusion quest
     (guild.exe  . ()) ; 60g secret store
     (yagostr.exe . (eden)) ; after gate of illusion shortcut

     ; items in the mausoleum of giants
     (rolling . ()) ; ghost king miniboss
     (hermes  . ()) ; store 60g
     (jewel2  . ()) ; sakit's gem with day/night/star puzzle
     (lifeup3 . ()) ; inside a foot
     (map3    . ()) ; floor switch puzzle

     ; items in temple of the sun
     (dagger       . ())
     (lifeup4      . ())
     (map4         . ()) ; do the pyramid runaround under grail tablet
     (jewel3       . ()) ; ellmac's gem
     (book-of-dead . (origin)) ; free mulbruk, talk to her for a bit, meet anubis, go back
     (mirror       . (origin))
     (pregnant     . (woman))
     (talisman     . (jewel5)) ; post ViY, statue crumbles

     ; items in the spring in the sky
     (map5        . ()) ; room after caltrops
     (origin      . (helmet)) ; elevator jump
     (scalesphere . (origin)) ; after miniboss
     (jewel4      . ()) ; whack-a-fish
     (lifeup5     . (birth)) ; left elevator 
     (glove       . ()) ; after unlocking the push block via spring puzzle
     (caltrops    . ()) ; transition glitch
     (randc.exe   . (birth)) ; birth

     ; inferno cavern
     (flare       . ()) ; transition glitch
     (map6        . ()) ; free
     (ice-cape    . ()) ; at the end
     (bunplus.com . ()) ; hidden rock spot by grail tab
     (capstar.exe . ()) ; 150g from store
     (chainwhip   . (birth claws)) ; after miniboss

     ; chamber of extinction
     (chakram . ()) ; free
     (life    . (birth)) ; dark platform jumping section
     (map7    . ())
     (lifeup7 . ())
     (mantra.exe . (magatama torude.exe)) ; use magatama jewel then scan

     ; twin labyrinth
     (katana     . (twin-statue))
     (ring       . ())
     (lifeup8    . ())
     (helmet     . ())
     (bracelet   . ()) ; 150g
     (dragonbone . (helmet)) ; kinda not but kinda is
     (time-lamp  . (grenades)) ; 200g
     (jewel7     . ())
     (map8       . ())

     ; endless corridor
     (map9     . ())
     (keyblade . ())
     (twin-statue . (infinity-key))

     ; shrine of mother (both versions)
     (map10 . ())
     (diary . (talisman))
     (death . (dragonbone yagomap.exe yagostr.exe))
     (crystal-skull . ())
     (bounce.exe . (infinity-key)) ; requires getting here

     ; flipside!!
     ; gate of illusion
     (pepper . (eden-fruit))
     (smalldoll . (treasure))
     (infinity-key . (smalldoll))

     ; graveyard of giants
     (grenades     . (plane)) ; you need plane to escape goddess tower
     (gauntlet     . (mirror life)) ; needs life seal to access
     (silvershield . (mirror)) ; freebie
     (mirai.exe    . (mirror)) ; freebie
     
     ; tower of ruin
     (spears  . ())
     (lifeup8 . (mirror)) ; enter thru graveyard
     (djed    . (death)) ; needs secret passage from birth->extinct->ruin
     (jewel5  . (mirror)) ; earliest way of getting here is thru graveyard

     ; items in temple of the moon
     (axe     . ())
     (serpent . (book-of-dead)) ; for feather, beat anubis
     (medicine . cup)

     ; tower of the goddess
     (plane        . ())
     (eye-of-truth . ())
     (flailwhip    . ())

     ; chamber of birth
     (woman . ()) 
     (cup   . (angelshield))

     ; dimensional corridor
     (angelshield . (crystal-skull))
     (jewel8 . ())

     ; The Secret Treasure of Life is the end goal item
     ; To get this item, you must beat Mother in her final "true" form
     ; Requires:
     ; * all four seals (Origin / Birth / Life / Death)
     ; * mind medicine
     ; * all eight bosses completed
     ; * the fairy queen fully unlocked
     ; * accessing "true" final shrine of awakening
     ; * the la-mulana talisman to dispel
     ; * dragonbone skull
     ; * keyblade to unlock the boss fight
     (treasure-of-life . (origin birth life death keyblade talisman dragonbone
                                 yagomap.exe yagostr.exe ; required for dragonbone
                                 infinity-key ; required to access area
                                 ,@(only-if-100% '(100%))
                                 ))

     ; 100% completionist "items" (groups of existing items)
     (all-store-items . ())
     (all-software . (randc.exe bounce.exe mirai.exe mekuri.exe
                                deathv.exe bunplus.exe capstar.exe
                                guild.exe))

     ; shortcut all maps into one item tag
     (all-maps . (map1 map2 map3 map4 map5 map6 map7 map8 map9
                       map10 map11 map12 map13 map14 map15 map16 map17))

     ; same, bind all lifeups into one tag
     (all-lifeups . (lifeup1 lifeup2 lifeup3 lifeup4 lifeup5
                             lifeup6 lifeup7 lifeup8 lifeup9))

     
     ; fairy vest - cuts damage in half, but not required (also kills achievements)
     ; waterproof laptop case - not necessary
     ; lavaproof laptop case - also not necessary
     ; bikini - after beating Hell Temple fully, not necessary
     ; shield - not required at all
     ; silver shield - an upgraded shield - not necessary though
     ; gun - a way to kill bosses by grinding, not necessary however
     ; grapple claws - technically not needed for anything progression wise
     (all-extra-items . (fairy-vest waterproof lavaproof bikini shield silvershield gun
                                    perfume batbook cross chainwhip flailwhip
                                    ))
     
     ; all items, including special bikini
     ; this includes software, extra items, weapons (gun), shields, etc
     (100%-completion . (all-store-items all-software all-extra-items))
     )))





(module+ main
  (command-line
   #:program "AnalyzeLM"
   #:args ()
   (begin
     (displayln "Welcome to La-Mulana"))))

; end
