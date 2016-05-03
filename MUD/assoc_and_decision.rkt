; File contains all the association lists, along with decision table for the
; MUD game

; Room description assoc list
(define descriptions '((1 "You have entered the dungeon! Tread carefully.")
                       (2 "Now you're in a hallway, seems to be two ways to go.")
                       (3 "You have entered a kitchen area. Looks like there's a storage area.")
                       (4 "It's the storage area.")
                       (5 "You've entered another hallway.")
                       (6 "Seems, youve entered a bedroom area.")
                       (7 "Looks like another bedroom.")
                       (8 "It's a living room.")
                       (9 "You've exited the bedroom and reached the balcony. Looks like there are stiars down.")
                       (10 "It's a courtyard. There a a few paths to take.")
                       (11 "You entered a green house.")
                       (12 "You go downstairs and find an armoury.")
                       (13 "You've entered an underground entrance.")
                       (14 "You went upstairs and hit a dead end.")
                       (15 "There's an exit!")))

; Objects assoc list
(define objects '((1 "A rusted coin")
                  (3 "A shiny fork")
                  (6 "A bronze amulet")
                  (7 "A silver braclet")
                  (10 "A silver chalice")
                  (11 "A shovel")
                  (12 "A long sword")
                  (14 "A GOLD COIN!")))


; Actions assoc list
(define look '(((directions) look) ((look) look) ((examine room) look)))
(define quit '(((exit game) quit) ((quit game) quit) ((exit) quit) ((quit) quit)))
(define pick '(((get) pick) ((pickup) pick) ((pick) pick)))
(define put '(((put) drop) ((drop) drop) ((place) drop) ((remove) drop)))
(define inventory '(((inventory) inventory) ((bag) inventory)))

; Get put into another list
; Quasiquoting the list, to give special properties
; List filled with unquote (,) Using unquote splicing ,@ so the extra list is removed
(define actions `(,@look ,@quit ,@pick ,@put ,@inventory))


; Decision table data helps drive the game, and what happens in each room
(define decisiontable `((1 ((north) 2) ,@actions)
                        (2 ((north east) 5) ((south) 1) ((east) 3) ,@actions)
                        (3 ((north) 5) ((south) 4) ((west) 2),@actions)
                        (4 ((north) 3) ,@actions)
                        (5 ((south) 3) ((south west) 2) ((east) 6) ,@actions)
                        (6 ((north) 8) ((south) 7) ((east) 9),@actions)
                        (7 ((north) 6) ,@actions)
                        (8 ((south) 6) ((south east) 9) ,@actions)
                        (9 ((north west) 8) ((west) 6) ((south) 10) ,@actions)
                        (10 ((north) 9) ((east) 13) ((north east) 14)
                            ((south) 11) ((south west) 12) ,@actions)
                        (11 ((north) 10) ((north east) 13) ,@actions)
                        (12 ((north east) 10) ,@actions)
                        (13 ((north) 14) ((west) 10) ((south west) 11)
                            ((east) 15) ,@actions)
                        (14 ((south) 13) ((south west) 10) ,@actions)
                        (15 ((west) 13) ,@actions)))