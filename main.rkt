#lang racket

; Tells the user the backstory to the game
(define (backstory-text)
  (displayln "You are driving on your way home from work when you get a phone call from your wife.")
  (displayln "She wants to make French Toast for breakfast tomorrow but doesn't have any time to stop by the store")
  (displayln "She asks you to get 4 things from the store on your way home: milk, eggs, bread, and sugar")
  (displayln "She tells you to only get the things on the list")
  (displayln "You arrive at the store\n")
  (displayln "You walk in through the Checkout. Theres a large sign stating \"WARNING shoplifters will be prosecuted\" with a camera above it. You can leave as soon as you pay for the groceries.")
  (displayln "You see the Fridge to the North, and the Bakery to the East."))

; Variable to store the initial state of the game
(define default-game-state (list
                            (list)                                     ; Inventory list
                            (list "Checkout")                          ; Players spawn location
                            (list "Bakery" '("Bread" "Sugar") #f)      ; Bakery state
                            (list "Fridge" '("Milk" "Eggs") #f)        ; Fridge state
                            (list "Freezer" '("Hidden Ice Cream") #f)  ; Freezer state
                            (list "Checkout" '() #t)))

; Function to tell the player what items they have in their inventory
; inv: current inveentory
; Returns: nothing, just prints to the user
(define (player-inv-menu inv)
  (if (empty? inv)
      (displayln "You have not gotten any groceries")
      (printf "You have: ~a in your cart\n" (string-join inv ", ")))"")

; Prints the map to the user in the console
; game-state: current game-state
; Returns: nothing, just prints the map to the console
(define (show-map game-state)
  (printf "You are currently in the ~a\n" (string-upcase (first (second game-state))))
  (displayln "Map:                                  N")
  (displayln "      Fridge    ←→    Freezer         ↑ ")
  (displayln "     ↗      ↖                     W ← + → E")
  (displayln "   ↙          ↘                       ↓ ")
  (displayln "Checkout ←→ Bakery                    S"))

; Function to describe a room to a player
; game-state: current game state
; Returns: nothing, just prints a description to the terminal
(define (describe-current-area-menu game-state)
  (for ([item (get-items-in-curr-room game-state)])
    (unless (string-contains? (string-downcase item) "hidden")
      (printf "You see ~a in here\n" item)))
  
  (cond
    [(string=? (first (second game-state)) "Bakery") (begin (displayln "You are at the Bakery. You see bread overflowing from the shelves, and a worker somehow adding more. You also see the Checkout, and the Fridge.")
                                                            (displayln "You see the Fridge to the Northwest, and the checkout to the West."))]
    [(string=? (first (second game-state)) "Fridge") (begin (displayln "You are in the Fridge. It's a little chilly. You see a bunch of milk and eggs lining the shelves.")
                                                            (displayln "You see the Freezer to the East, the Checkout to the Southwest, and the Bakery to the Southeast."))]
    [(string=? (first (second game-state)) "Freezer") (begin (displayln "You are in the Freezer. All of the upper shelves are empty...")
                                                             (displayln "You see a door to the East leading to the Fridge."))]
    [(string=? (first (second game-state)) "Checkout") (begin (displayln "You are at the Checkout. Theres a large sign stating \"WARNING shoplifters will be prosecuted\" with a camera above it. You can leave as soon as you pay for the groceries.")
                                                              (displayln "You see the Fridge to the North, and the Bakery to the East."))]))

; Function to show the user what actions are possible
; game-state: current game state
; Returns: nothing, just prints to console
(define (show-actions game-state)
  (printf "You are currently at the ~a\n" (first (second game-state)))
  (printf "~a" (player-inv-menu (first game-state)))
  (displayln "Available moves are:")
  (help-menu))


; Function to search the room (behind other groceries)
; game-state: current game state
; Returns: updated game state
(define (search-current-area game-state)
  ; Gets all the hidden marked items
  (define hidden-list (filter (lambda (item)
                                (string-contains? (string-downcase item) "hidden"))
                              (get-items-in-curr-room game-state)))

  (if (empty? hidden-list)
      (displayln "Nothing new was uncovered in the search")
      (for ([item hidden-list])
        (printf "You find ~a in the search\n" (substring item 7 (string-length item)))))

  (update-game-state (first (second game-state)) game-state (list (first (second game-state)) (map (lambda (str) (string-replace str "hidden " ""))(get-items-in-curr-room game-state)) #t) "" (add-to-inv (first game-state) "")))

; Function to help the user, will tell them what they can do
; Returns: nothing
(define (help-menu)
  (displayln "You can move with \"Move\" and a direction (N, NE, E, SE, S, SW, W, NW)\t\tEx. \"Move N\"")
  (displayln "You can pickup and drop items with \"Pickup\" or \"Drop\" and an item name\t\tEx. \"Pickup ice cream\", or \"Drop ice cream\"")
  (displayln "You can see the map and your location with \"Show map\"\t\t\t\tEx. \"Show map\"")
  (displayln "You can look into your cart with \"Show cart\"\t\t\t\t\tEx. \"Show cart\"")
  (displayln "You can use \"Where am I?\" to find more information about the current area\tEx. \"Where am I?\", or \"Where am I\"")
  (displayln "You can search the area with \"Search\"\t\t\t\t\t\tEx. \"Search\"")
  (displayln "You can leave the game with \"Leave\" when you are in the checkout\t\tEx. \"Leave\""))

; Function to add an item to the inventory
; inv: current inventory
; item: item to ad to the inventory
; Returns: an updated inventory
(define (add-to-inv inv item)
  (if (not (string=? "" item))
      (cons item inv)
      inv))

; Function to remove an item from the player's inv
; inv: current inventory
; item-name: name of item to remove from inventory
; Returns: an updated inventory
(define (rem-from-inv inv item-name)
  (if (= (length inv) 1)
      '() ; if only one item in list, return an empty list
      (filter (lambda (elem) (not (string=? elem item-name))) (map string-downcase inv))))

; Function to get all items in a room
; game-state: current game state
; Return: list of all items in the current room in lowercase
(define (get-items-in-curr-room game-state)
  (define curr-room (first (second game-state)))  ; Where the user currently is
  (define all-items (cond
                      [(string=? curr-room "Bakery") (map string-downcase (second (third game-state)))]
                      [(string=? curr-room "Fridge") (map string-downcase (second (fourth game-state)))]
                      [(string=? curr-room "Freezer") (map string-downcase (second (fifth game-state)))]
                      [(string=? curr-room "Checkout") (map string-downcase (second (sixth game-state)))]))

  
  all-items)  ; Returns the items in the current room

; Function to put an item on the ground
; item-name: name of the item to drop
; game-state: game state
; inv-check: true or false whether or not to check if item is in current inv
; Returns an updated game-state
(define (drop-item item-name game-state inv-check)

  (if (or (member item-name (first game-state)) (string=? "false" inv-check))
      ; If the item to drop is in your inventory, proceed otherwise spit an error
      (cond  ; If the item-name is in the current room, pick it up, else display an error
        ; If the item is in your inv, remove it and drop it on the floor                                                     ; current room                 ; adds item to room items      ; sets to the same visited
        [(string=? "Bakery" (first (second game-state))) (update-game-state (first (second game-state)) game-state (list (first (second game-state)) (cons item-name (second (third game-state))) (third (third game-state))) item-name (rem-from-inv (first game-state) item-name))]
        [(string=? "Fridge" (first (second game-state))) (update-game-state (first (second game-state)) game-state (list (first (second game-state)) (cons item-name (second (fourth game-state))) (third (third game-state))) item-name (rem-from-inv (first game-state) item-name))]
        [(string=? "Freezer" (first (second game-state))) (update-game-state (first (second game-state)) game-state (list (first (second game-state)) (cons item-name (second (fifth game-state))) (third (third game-state))) item-name (rem-from-inv (first game-state) item-name))]
        [(string=? "Checkout" (first (second game-state))) (update-game-state (first (second game-state)) game-state (list (first (second game-state)) (cons item-name (second (sixth game-state))) (third (third game-state))) item-name (rem-from-inv (first game-state) item-name))]
        [else (begin (printf "\"~a\" is not in your inventory" item-name) game-state)])
      (begin (printf "\"~a\" is not in your inventory" item-name) game-state))); Tells the user the item is not in their inv and returns the current game state

; Function to pickup an item
; item-name: name of the item the user wants to pick up
; game-state: the current game-state
(define (pickup-item item-name game-state)
  (cond  ; If the item-name is in the current room, pick it up, else display an error
    ;                                                                                      ; removes the item from the room
    [(and (member item-name (get-items-in-curr-room game-state)) (not (string-contains? item-name "hidden"))) (begin (printf "You picked up ~a\n" item-name) (remove-item-from-room game-state item-name))]
    [else (begin (printf "You did not find \"~a\"" item-name) game-state)]))

; Function to remove an item from the room
; game-state: takes the current game state
; item-name: name of item to remove from room
; Returns: updated game state
(define (remove-item-from-room game-state item-name)
  (cond
    [(string=? (first (second game-state)) "Bakery") (update-game-state "Bakery" game-state (list "Bakery" (filter (lambda (elem) (not (string=? elem item-name))) (map string-downcase (second (third game-state)))) #t) item-name (add-to-inv (first game-state) item-name))]
    [(string=? (first (second game-state)) "Fridge") (update-game-state "Fridge" game-state (list "Fridge" (filter (lambda (elem) (not (string=? elem item-name))) (map string-downcase (second (fourth game-state)))) #t) item-name (add-to-inv (first game-state) item-name))]
    [(string=? (first (second game-state)) "Freezer") (update-game-state "Freezer" game-state (list "Freezer" (filter (lambda (elem) (not (string=? elem item-name))) (map string-downcase (second (fifth game-state)))) #t) item-name (add-to-inv (first game-state) item-name))]
    [(string=? (first (second game-state)) "Checkout") (update-game-state "Checkout" game-state (list "Checkout" (filter (lambda (elem) (not (string=? elem item-name))) (map string-downcase (second (sixth game-state)))) #t) item-name (add-to-inv (first game-state) item-name))]))

; Function to take apart the old state and replace the one list with the new state
; list-to-update: the section of game-state to update
; old state is the whole game state
; new state is the single list we want to update
; item-name: ??
; lmb inv; lambda function to add or remove from inv
; Returns: an updated game state
(define (update-game-state list-to-update old-state new-state item-name lmb-inv)
  (cond
    [(string=? list-to-update "inv") (list new-state (second old-state) (third old-state) (fourth old-state) (fifth old-state) (sixth old-state))]  ; Takes the new inventory and the rest of the old state and combines them back into the game-state
    [(string=? list-to-update "location") (list (first old-state) (list new-state) (third old-state) (fourth old-state) (fifth old-state) (sixth old-state))]
    [(string=? list-to-update "Bakery") (list lmb-inv (second old-state) new-state (fourth old-state) (fifth old-state) (sixth old-state))]
    [(string=? list-to-update "Fridge") (list lmb-inv (second old-state) (third old-state) new-state (fifth old-state) (sixth old-state))]
    [(string=? list-to-update "Freezer") (list lmb-inv (second old-state) (third old-state) (fourth old-state) new-state (sixth old-state))]
    [(string=? list-to-update "Checkout") (list lmb-inv (second old-state) (third old-state) (fourth old-state) (fifth old-state) new-state)]
    [else old-state]))

; Function to check if the user has met the leave conditions
; game-state: current game state
; Returns: list of game state and true/false valid leave condition
(define (check-leave-cond game-state)  ; Checks the users inventory and if they have all the necessary items 
  (if (string=? (first (second game-state)) "Checkout")
      (cond
        [(and (member "bread" (first game-state)) (member "milk" (first game-state)) (member "eggs" (first game-state)) (member "sugar" (first game-state)) (member "ice cream" (first game-state))) (begin (displayln "The End.\nYou got all of the ingredients on the list, and your wife is excited that you brought her home ice cream.") (list game-state "true"))]
        [(and (member "bread" (first game-state)) (member "milk" (first game-state)) (member "eggs" (first game-state)) (member "sugar" (first game-state))) (begin (displayln "The End.\nYou got everything on the list, and woke up to French Toast the next morning.") (list game-state "true"))]
        [(or (not (member "bread" (first game-state))) (not (member "milk" (first game-state))) (not (member "eggs" (first game-state))) (not (member "sugar" (first game-state)))) (begin (displayln "You do not have everything on the list") (list game-state "false"))])
      (displayln "You have to be at the checkout to leave")))

; Function to allow the player to move
; direction: the direction the user inputs to move
; game-state: current game state
; Returns: updated game state after the move, or appends false to the list to show player is in the same room
(define (move-player direction game-state)
  ; List of valid moves
  (define valid-moves (list "move n" "move ne" "move e" "move se" "move s" "move sw" "move w" "move nw"))
  (if (not (not (member direction valid-moves)))
      (cond ; If in the checkout and moving east, enter the bakery and update the game state
        [(and (string=? direction "move e") (string=? (first (second game-state)) "Checkout")) (update-game-state "location" game-state "Bakery" "" "")]
        [(and (or (string=? direction "move n") (string=? direction "move ne")) (string=? (first (second game-state)) "Checkout")) (update-game-state "location" game-state "Fridge" "" "")]
        [(and (string=? direction "move e") (string=? (first (second game-state)) "Fridge")) (update-game-state "location" game-state "Freezer" "" "")]
        [(and (string=? direction "move sw") (string=? (first (second game-state)) "Fridge")) (update-game-state "location" game-state "Checkout" "" "")]
        [(and (string=? direction "move se") (string=? (first (second game-state)) "Fridge")) (update-game-state "location" game-state "Bakery" "" "")]
        [(and (string=? direction "move w") (string=? (first (second game-state)) "Bakery")) (update-game-state "location" game-state "Checkout" "" "")]
        [(and (or (string=? direction "move nw") (string=? direction "move n")) (string=? (first (second game-state)) "Bakery")) (update-game-state "location" game-state "Fridge" "" "")]
        [(and (string=? direction "move w") (string=? (first (second game-state)) "Freezer")) (update-game-state "location" game-state "Fridge" "" "")]
        [else (begin (displayln "There's no door over here...") (list game-state "false"))])
      (begin (displayln "Please enter a valid direction") (list game-state "false"))))
  
; Function to take in the game state and check if youve entered the room before, if you have then print the short description, else print the long one
; game-state: current game state
; Returns: displays proper description to the user, and returns a game state where the current room has been visited
(define (visited-location? game-state)
  ; if the room has not changed between moves do not update game state
  (if (not (equal? (last game-state) "false"))
      (begin
        (cond
          ; if youre in the bakery and its been visited before, print short dialogue
          [(and (string=? "Bakery" (first (second game-state))) (member #t (third game-state))) (displayln "You've entered the Bakery, it looks familiar.")]
          [(and (string=? "Fridge" (first (second game-state))) (member #t (fourth game-state))) (displayln "You've entered the Fridge, it looks familiar.")]
          [(and (string=? "Freezer" (first (second game-state))) (member #t (fifth game-state))) (displayln "You've entered the Freezer, it looks familiar.")]
          [(and (string=? "Checkout" (first (second game-state))) (member #t (sixth game-state))) (displayln "You've entered the Checkout, it looks familiar.")]
          [(and (string=? "Bakery" (first (second game-state))) (member #f (third game-state))) (displayln "You are at the Bakery. You see bread overflowing from the shelves, and a worker somehow adding more. You also see the Checkout, and the Fridge.")]
          [(and (string=? "Fridge" (first (second game-state))) (member #f (fourth game-state))) (displayln "You are in the Fridge. It's a little chilly. You see a bunch of milk and eggs lining the shelves, and a door to the Freezer.")]
          [(and (string=? "Freezer" (first (second game-state))) (member #f (fifth game-state))) (displayln "You are in the Freezer. There's Ice Cream lining the shelves, and the only way out is back through the Fridge.")])
        (cond
          ; If you're in the checkout mark it visited
          [(string=? "Bakery" (first (second game-state))) (update-game-state "Bakery" game-state (list "Bakery" (second (third game-state)) #t) "" (add-to-inv (first game-state) ""))]
          [(string=? "Fridge" (first (second game-state))) (update-game-state "Fridge" game-state (list "Fridge" (second (fourth game-state)) #t) "" (add-to-inv (first game-state) ""))]
          [(string=? "Freezer" (first (second game-state))) (update-game-state "Freezer" game-state (list "Freezer" (second (fifth game-state)) #t) "" (add-to-inv (first game-state) ""))]
          [(string=? "Checkout" (first (second game-state))) (update-game-state "Checkout" game-state (list "Checkout" (second (sixth game-state)) #t) "" (add-to-inv (first game-state) ""))]))
      (first game-state)))

; Function to prompt again or quit
; valid: validation of whether or not to prompt the user to play again
; Returns: resarts the game or quits the application
(define (again? big-list)

  (cond
    [(string=? (second big-list) "true") (begin
                                           (displayln "Would you like to play again? (Y/N)")
                                           (define user-input (string-trim (string-downcase (read-line))))  ; Reads the input from the user
                                           (cond
                                             [(string=? (string-downcase user-input) "y") (game-loop default-game-state)]
                                             [(string=? (string-downcase user-input) "n") (exit 0)]
                                             [else (displayln "Please enter a valid option (Y/N)")]))]
    [else (first big-list)]))

; Function to loop through prompting the user and processing input
; game-state: current game state
; Returns: Recursive call to itself
(define (game-loop game-state)  ; Main game loop that will take the users input and process it
  (displayln "\nWhat would you like to do?")  ; Prompts the user to make a move
  (define user-input (string-trim (string-downcase (read-line))))  ; Reads the input from the user
  
  (cond  ; Cond loop to test what the user input and process the request
    [(string-prefix? user-input "move")  (game-loop (visited-location? (move-player user-input game-state)))]
    [(string=? user-input "show map") (show-map game-state)]
    [(string=? user-input "show cart") (player-inv-menu (first game-state))]
    [(or (string=? user-input "where am i") (string=? user-input "where am i?")) (describe-current-area-menu game-state)]
    [(string-prefix? user-input "pickup") (game-loop (pickup-item (substring user-input 7 (string-length user-input)) game-state))]
    [(string-prefix? user-input "drop") (game-loop (drop-item (substring user-input 5 (string-length user-input)) game-state "true"))]
    [(string=? user-input "leave") (again? (check-leave-cond game-state))]
    [(string=? user-input "search") (game-loop (search-current-area game-state))]
    [(string=? user-input "help") (show-actions game-state)]
    [else (begin (displayln "Invalid command.") (help-menu))])  ; Give the user the help menu since they did not input a valid move
  (game-loop game-state))

; Launches the game with an empty state
; the default game state is a list of lists where each nested sublist in order, is:
; 1: the players current inventory, initialized blank
; 2: the players current location, initalized to the checkout
; 3,4,5 and 6: the location, it contains the name of the area, the items that are in that area, and if the room has been previously visited
(backstory-text)
(game-loop default-game-state)

