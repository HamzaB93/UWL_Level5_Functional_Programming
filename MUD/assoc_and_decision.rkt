; File contains all the association lists, along with decision table for the
; MUD game

; Association list for room descriptions
(define descriptions
  '((1 "You have entered the dungeon! Tread carefully.")
    (2 "Entered a small box room.")
    (3 "This is the east side of a courtyard.")
    (4 "It's the west side of a courtyard.")
    (5 "You've stumbled across an entrance to a castle.")
    (6 "This is the hall of the castle.")
    (7 "Looks like an armoury.")
    (8 "It looks like a leasure area.")
    (9 "This is a massive dining room.")
    (10 "You reached the end. You can exit the dungeon or go back.")))

; Association list for Objects that belong to certain rooms
(define objects
  '((1 "A pebble")
    (3 "A small knife")
    (6 "A silver chalice")
    (7 "A long sword")
    (10 "A gold coin")))


; Actions assoc list, where keywords will result in an action
(define look '(((directions) look) ((look) look) ((examine room) look)))
(define quit '(((exit game) quit) ((quit game) quit) ((exit) quit) ((quit) quit)))
(define pick '(((get) pick) ((pickup) pick) ((pick) pick)))
(define put '(((put) drop) ((drop) drop) ((place) drop) ((remove) drop)))
(define inventory '(((inventory) inventory) ((bag) inventory)))

; Quasiquoting the list, to give special properties
; List filled with unquote (,) Using unquote splicing ,@ so the extra list is removed
(define actions `(,@look ,@quit ,@pick ,@put ,@inventory))


; Decision table data helps drive the game, and what happens in each room
; Each room will allow all actions to take place
(define decisiontable
  `((1 ((east) 2) ((north east) 3) ((north west) 4) ,@actions)
    (2 ((west) ,@actions))
    (3 ((south west) 1) ((north west) 5) ,@actions)
    (4 ((south east) 1) ((north east) 5) ,@actions)
    (5 ((north) 6) ((south west) 4) ((south east) 3) ,@actions)
    (6 ((west) 8) ((south) 5) ((east) 7) ,@actions)
    (7 ((west) 6) ((north west) 9) ,@actions)
    (8 ((est) 6) ((north east) 9) ,@actions)
    (9 ((south west) 8) ((south east) 7) ((north) 10) ,@actions)
    (10 ((south) 9) ,@actions)))