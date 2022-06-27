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

(define nil '())

; Basic graph structure w/ node container
(struct Node (id proper-name requireds))
(struct Graph (nodes edges))


; usable with foldl/foldr
(define (Graph-add-node node G)
  (let ([node-hash (Graph-nodes G)]
        [nid (Node-id node)])
    (Graph
     (if (hash-has-key? node-hash nid)
         G
         (hash-update node-hash nid node))
     (Graph-edges G))))

(define (Graph-init #:starting-nodes [sn '()])
  (Graph (make-immutable-hash '()) '()))

  
; This is the complete and total graph of items in La-Mulana
; All NECESSARY items are here
; Unnecessary items are tracked, but are not required for end-of-game
; A setting can be toggled on to include them as needed
(define/contract (Build-Graph)
  (-> Graph?)
  ;; write all items out in Graph Node form with symbol * string * list
  (define all-items
    `(; surface
      (Node scanner     "Hand Scanner"      nil)
      (Node shell       "Shell Horn"        nil)
      (Node feather     "Feather"           '(serpent))
      (Node birth       "Birth Seal"        '(origin))
      (Node lifeup1     "Life Orb (Surface)" nil) ; technically possible
      (Node map1        "Map (Surface)"     nil)
      (Node bucker      "Buckler"           nil)
      (Node waterproof  "Waterproof Case"   nil)
      (Node laptop2     "MSX 5000"          '(jewel1 jewel2 jewel3 jewel4))
      (Node mulana      "Mulana Talisman"   '(diary))
      (Node reader.exe  "reader.exe"        nil)
      (Node deathv.exe  "deathv.exe"        nil)
      (Node yagomap.exe "yagomap.exe"       nil)
      (Node mekuri.exe  "mekuri.exe"        nil)
      ; gate of guidance
      (Node grail       "Holy Grail"                  nil)
      (Node shuriken    "Shuriken"                    nil)
      (Node jewel1      "Ankh Jewel (Amphisbaena)"    nil)
      (Node lifeup2     "Life Orb (Gate of Guidance)" nil)
      (Node map2        "Map (Gate of Guidance)"      nil)
      (Node crucifix    "Crucifix"                    '(flare life))
      (Node treasure    "Treausres"                   '(pepper))
      (Node guild.exe   "guild.exe"                   nil)
      (Node yagostr.exe "yagostr.exe"                 '(eden))
      ; mausoleum of giants
      (Node rolling     "Rolling Shuriken"  nil)
      (Node hermes      "Hermes' Boots"     nil)
      (Node jewel2      "Ankh Jewel (Sakit)" nil)
      (Node lifeup3     "Life Orb (Mausoleum of Giants)" nil)
      (Node map3        "Map (Mausoleum of Giants)" nil)
      ; temple of the sun
      (Node knife "Knife" nil)
      (Node lifeup4 "Life Orb (Temple of the Sun)" nil)
      (Node map4 "Map (Temple of the Sun)" nil)
      (Node jewel3 "Ankh Jewel (Ellmac)" nil)
      (Node book-of-dead "Book of the Dead" '(origin))
      (Node mirror "Bronze Mirror" '(origin))
      (Node pregnant "Pregnant Woman Statue" '(woman))
      (Node talisman "Talisman" '(jewel5))
      ; spring in the sky
      (Node map5 "Map (Spring in the Sky)" nil)
      (Node origin "Origin Seal" nil)
      (Node scalesphere "Scalesphere" '(origin))
      (Node jewel4 "Ankh Jewel (Bahamut)" '(origin)) ; origin req'd to activate ankh
      (Node lifeup5 "Life Orb (Spring in the Sky)" '(birth))
      (Node glove "Gloves" nil)
      (Node caltrops "Caltrops" nil) ; transition glitch
      (Node randc.exe "randc.exe" '(birth))
      ; inferno cavern
      (Node flare "Flare Gun" nil)
      (Node map6 "Map (Inferno Cavern)" nil)
      (Node ice-cape "Ice Cape" nil)
      (Node bunplus.exe "bunplus.exe" nil)
      (Node capstar.exe "capstar.exe" nil)
      (Node chainwhip "Chain Whip" '(birth claws))
      ; chamber of extinction
      (Node chakram "Chakram" nil)
      (Node life "Life Seal" '(birth))
      (Node map7 "Map (Chamber of Extinction)" nil)
      (Node lifeup7 "Life Orb (Chamber of Extinction)" nil)
      (Node mantra.exe "mantra.exe" '(magatama torude.exe))
      ; twin labyrinth
      (Node katana "Katana" '(twin-statue))
      (Node ring "Ring" '(twin-statue))
      (Node lifeup8 "Life Orb (Twin Labyrinths)" '(twin-statue))
      (Node helmet "Helmet" nil)
      (Node dragonbone "Dragon Bone" '(helmet)) ; not technically, but realistically
      (Node time-lamp "Time Lamp" '(grenades))
      (Node jewel7 "Ankh Jewel (Baphomet)" '(twin-statue))
      (Node map8 "Map (Twin Labyrinths)" '(twin-statue))
      ; endless corridor
      (Node map9 "Map (Endless Corridor)" nil)
      (Node keyblade "Key Blade" '(infinity-key))
      (Node twin-statue "Twin Statue" '(infinity-key))
      ))
  (make-immutable-hash '(
     ; chamber of extinction
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
