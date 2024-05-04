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
                            (list)  ; Spawn with an empty inventory
                            (list "Checkout")  ; Spawn in the Checkout
                            (list "Bakery" '("Bread" "Sugar") #f)  ; Name of area, items in that room, and if the room has been visited
                            (list "Fridge" '("Milk" "Eggs") #f)  ; Name of area, items in that room, and if the room has been visited
                            (list "Freezer" '("Hidden Ice Cream") #f)  ; Name of area, items in that room, and if the room has been visited
                            (list "Checkout" '() #t)))  ; Name of area, nothing spawning at checkout, and room was visited because we spawn there

; Function to tell the player what items they have in their inventory
; inv: current inventory
; Returns: "" because it will print #<void> otherwise, just prints to the user
(define (player-inv-menu inv)
  (if (empty? inv)  ; Tests if there are groceries in inv and display if so
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
  (define curr-room (first (second game-state)))  ; Current room
  (for ([item (get-items-in-curr-room game-state)])  ; Goes through all items in the curr-room
    (unless (string-contains? (string-downcase item) "hidden")  ; Do not print hidden items
      (printf "You see ~a in here\n" item)))
  (cond  ; Cond for displaying the right text for each area
    [(string=? curr-room "Bakery") (begin (displayln "You are at the Bakery. You see bread overflowing from the shelves, and a worker somehow adding more. You also see the Checkout, and the Fridge.")
                                          (displayln "You see the Fridge to the Northwest, and the checkout to the West."))]
    [(string=? curr-room "Fridge") (begin (displayln "You are in the Fridge. It's a little chilly. You see a bunch of milk and eggs lining the shelves.")
                                          (displayln "You see the Freezer to the East, the Checkout to the Southwest, and the Bakery to the Southeast."))]
    [(string=? curr-room "Freezer") (begin (displayln "You are in the Freezer. All of the upper shelves are empty...")
                                           (displayln "You see a door to the East leading to the Fridge."))]
    [(string=? curr-room "Checkout") (begin (displayln "You are at the Checkout. Theres a large sign stating \"WARNING shoplifters will be prosecuted\" with a camera above it. You can leave as soon as you pay for the groceries.")
                                            (displayln "You see the Fridge to the North, and the Bakery to the East."))]))

; Function to show the user what actions are possible
; game-state: current game state
; Returns: nothing, just prints to console
(define (show-actions game-state)
  (printf "You are currently at the ~a\n" (first (second game-state)))  ; print current area
  (printf "~a" (player-inv-menu (first game-state)))  ; prints all items in your inv
  (displayln "Available moves are:")  ; shows available moves
  (help-menu))

; Function to search the room (behind other groceries)
; game-state: current game state
; Returns: updated game state
(define (search-current-area game-state)
  (define curr-room (first (second game-state)))  ; Current room
  (define hidden-list (filter (lambda (item) (string-contains? (string-downcase item) "hidden")) (get-items-in-curr-room game-state)))  ; Gets all the hidden marked items
  (if (empty? hidden-list)  ; Will tell the user what was uncovered in the search, if anything
      (displayln "Nothing new was uncovered in the search")
      (for ([item hidden-list])  ; goes through the hidden items and uncovers the item
        (printf "You find ~a in the search\n" (substring item 7 (string-length item)))))
  (update-game-state curr-room game-state (list curr-room (map (lambda (str) (string-replace str "hidden " "")) (get-items-in-curr-room game-state)) #t) "" (add-to-inv game-state "")))  ; Returns the updated game state

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
(define (add-to-inv game-state item)
  (define inv (first game-state))  ; Converts game state to inv
  (if (not (string=? "" item))  ; Used for searching an area because we dont actually want to add it to inv
      (cons item inv)  ; Adds the item to the inv
      inv))

; Function to remove an item from the player's inv
; inv: current inventory
; item-name: name of item to remove from inventory
; Returns: an updated inventory
(define (rem-from-inv inv item-name)
  (if (= (length inv) 1)
      '() ; if only one item in list, return an empty list
      (filter (lambda (elem) (not (string=? elem item-name))) (map string-downcase inv))))  ; If multiple items in inv, remove the specific one

; Function to get all items in a room
; game-state: current game state
; Return: list of all items in the current room in lowercase
(define (get-items-in-curr-room game-state)
  (define curr-room (first (second game-state)))  ; Where the user currently is
  (define room-list (list "Bakery" "Fridge" "Freezer" "Checkout"))  ; List of rooms to easier calculate list-ref later
  (map string-downcase (second (list-ref game-state (+ (index-of room-list curr-room) 2)))))  ; Returns the items in the current room

; Function to put an item on the ground
; item-name: name of the item to drop
; game-state: game state
; inv-check: true or false whether or not to check if item is in current inv
; Returns an updated game-state
(define (drop-item item-name game-state inv-check)
  (define curr-room (first (second game-state)))  ; Current room
  (define room-list (list "Bakery" "Fridge" "Freezer" "Checkout"))  ; List of rooms to easier calculate list-ref later
  (if (member item-name (first game-state))  ; If we just want to drop item (used for searching)
      ; If the item to drop is in your inventory, proceed otherwise spit an error
      (begin (printf "You have dropped \"~a\"\n" item-name) (update-game-state curr-room game-state (list curr-room (cons item-name (second (list-ref game-state (+ (index-of room-list curr-room) 2)))) #t) item-name (rem-from-inv (first game-state) item-name)))  ; Generic function to remove an item from inv for all rooms
      (begin (printf "\"~a\" is not in your inventory\n" item-name) game-state))); Tells the user the item is not in their inv and returns the current game state

; Function to pickup an item
; item-name: name of the item the user wants to pick up
; game-state: the current game-state
(define (pickup-item item-name game-state)
  (cond  ; If the item-name is in the current room, pick it up, else display an error
    [(and (member item-name (get-items-in-curr-room game-state)) (not (string-contains? item-name "hidden")))
     (begin (printf "You picked up ~a\n" item-name) (remove-item-from-room game-state item-name))]  ; Will pick up a specified non hidden item
    [else (begin (printf "You did not find \"~a\"" item-name) game-state)]))

; Function to remove an item from the room
; game-state: takes the current game state
; item-name: name of item to remove from room
; Returns: updated game state
(define (remove-item-from-room game-state item-name)
  (define curr-room (first (second game-state)))  ; Where the user currently is
  (define room-list (list "Bakery" "Fridge" "Freezer" "Checkout"))  ; List of rooms to easier calculate list-ref later
  (update-game-state curr-room game-state (list curr-room (filter (lambda (elem) (not (string=? elem item-name))) (map string-downcase (second (list-ref game-state (+ (index-of room-list curr-room) 2))))) #t) item-name (add-to-inv game-state item-name)))  ; Generic function to rem item from rooms using the room-list from earlier

; Function to take apart the old state and replace the one list with the new state
; list-to-update: the section of game-state to update
; old state is the whole game state
; new state is the single list we want to update
; item-name: used for dropping items
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

; Function to allow the player to move
; direction: the direction the user inputs to move
; game-state: current game state
; Returns: updated game state after the move, or appends false to the list to show player is in the same room
(define (move-player direction game-state)
  (define valid-moves (list "move n" "move ne" "move e" "move se" "move s" "move sw" "move w" "move nw"))  ; List of valid moves
  (define curr-room (first (second game-state)))  ; Current room
  (if (not (not (member direction valid-moves)))  ; if not a valid move, tell the user
      (cond ; If in the checkout and moving east, enter the bakery and update the game state
        [(and (string=? direction "move e") (string=? curr-room "Checkout")) (update-game-state "location" game-state "Bakery" "" "")]
        [(and (or (string=? direction "move n") (string=? direction "move ne")) (string=? curr-room "Checkout")) (update-game-state "location" game-state "Fridge" "" "")]
        [(and (string=? direction "move e") (string=? curr-room "Fridge")) (update-game-state "location" game-state "Freezer" "" "")]
        [(and (string=? direction "move sw") (string=? curr-room "Fridge")) (update-game-state "location" game-state "Checkout" "" "")]
        [(and (string=? direction "move se") (string=? curr-room "Fridge")) (update-game-state "location" game-state "Bakery" "" "")]
        [(and (string=? direction "move w") (string=? curr-room "Bakery")) (update-game-state "location" game-state "Checkout" "" "")]
        [(and (or (string=? direction "move nw") (string=? direction "move n")) (string=? curr-room "Bakery")) (update-game-state "location" game-state "Fridge" "" "")]
        [(and (string=? direction "move w") (string=? curr-room "Freezer")) (update-game-state "location" game-state "Fridge" "" "")]
        [else (begin (displayln "There's no door over here...") (list game-state "false"))])
      (begin (displayln "Please enter a valid direction") (list game-state "false"))))
  
; Function to take in the game state and check if youve entered the room before, if you have then print the short description, else print the long one
; game-state: current game state
; Returns: displays proper description to the user, and returns a game state where the current room has been visited
(define (visited-location? game-state)
  (define room-list (list "Bakery" "Fridge" "Freezer" "Checkout"))  ; Helps calculate the list-ref of the inventory items later
  (if (not (equal? (last game-state) "false"))  ; if the room has not changed between moves do not update game state
      (begin (cond  ; if youre in an area and its been visited before, print short dialogue, else print the long description
               [(string=? "Bakery" (first (second game-state))) (if (equal? #t (third (third game-state))) (displayln "You've entered the Bakery, it looks familiar.") (displayln "You are at the Bakery. You see a whole shelf with different kinds of sugar. You also see bread overflowing from the shelves, and a worker somehow adding more. You also see the Checkout, and the Fridge."))]
               [(string=? "Fridge" (first (second game-state))) (if (equal? #t (third (fourth game-state))) (displayln "You've entered the Fridge, it looks familiar.") (displayln "You are in the Fridge. It's a little chilly. You see a bunch of milk and eggs lining the shelves, and a door to the Freezer."))]
               [(string=? "Freezer" (first (second game-state))) (if (equal? #t (third (fifth game-state))) (displayln "You've entered the Freezer, it looks familiar.") (displayln "You are in the Freezer. All of the upper shelves are empty... The only way out is back through the Fridge."))]
               [(string=? "Checkout" (first (second game-state))) (displayln "You've entered the Checkout, it looks familiar.")])  ; Dont need to check the Checkout because you spawn here
             (update-game-state (first (second game-state)) game-state (list (first (second game-state)) (second (list-ref game-state (+ (index-of room-list (first (second game-state))) 2))) #t) "" (add-to-inv game-state "")))  ; Generic function to update all rooms using the room-list from earlier
      (first game-state)))  ; If youre in the same area after an attempted move, send the same state back to the user.

; Function to check if the user has met the leave conditions
; game-state: current game state
; Returns: list of game state and true/false valid leave condition
(define (check-leave-cond game-state)  ; Checks the users inventory and if they have all the necessary items
  (define required-items (list "bread" "sugar" "milk" "eggs"))
  (define alt-required-items (list "bread" "sugar" "milk" "eggs" "ice cream"))
  (if (string=? (first (second game-state)) "Checkout")
      (cond
        [(equal? (not (not (and (= (length (first game-state)) (length alt-required-items)) (andmap (lambda (item) (member item alt-required-items equal?)) (first game-state))))) #t)  ; Tests for hidden ending
         (begin (displayln "The End.\nYou got all of the ingredients on the list, and your wife is excited that you brought her home ice cream.") (list game-state "true"))]
        [(equal? (not (not (and (= (length (first game-state)) (length required-items)) (andmap (lambda (item) (member item required-items equal?)) (first game-state))))) #t)  ; Tests for regular ending
         (begin (displayln "The End.\nYou got everything on the list, and woke up to French Toast the next morning.") (list game-state "true"))]
        [else (begin (displayln "You do not have everything on the list") (list game-state "false"))])   ; if not have all required items
      (begin (displayln "You have to be at the checkout to leave") (list game-state "false"))))

; Function to prompt again or quit
; valid: validation of whether or not to prompt the user to play again
; Returns: resarts the game or quits the application
(define (again? big-list)
  (cond
    [(string=? (second big-list) "true")  ; If the user should be prompted to play again, prompt and process their response
     (begin
       (displayln "Would you like to play again? (Y/N)")
       (define user-input (string-trim (string-downcase (read-line))))  ; Reads the input from the user
       (cond  ; processes the users input of playing again or not
         [(string=? (string-downcase user-input) "y") (game-loop default-game-state)]
         [(string=? (string-downcase user-input) "n") (exit 0)]
         [else (displayln "Please enter a valid option (Y/N)")]))]
    [else (first big-list)]))  ; will return the regular state if not ready to leave

; Function to loop through prompting the user and processing input
; game-state: current game state
; Returns: Recursive call to itself
(define (game-loop game-state)  ; Main game loop that will take the users input and process it
  (displayln "\nWhat would you like to do?")  ; Prompts the user to make a move
  (define user-input (string-trim (string-downcase (read-line))))  ; Reads the input from the user
  (cond  ; Cond loop to test what the user input and process the request
    [(string-prefix? user-input "move") (game-loop (visited-location? (move-player user-input game-state)))]
    [(string=? user-input "show map") (show-map game-state)]
    [(string=? user-input "show cart") (player-inv-menu (first game-state))]
    [(or (string=? user-input "where am i") (string=? user-input "where am i?")) (describe-current-area-menu game-state)]
    [(string-prefix? user-input "pickup")(game-loop (pickup-item (substring user-input 7 (string-length user-input)) game-state))]
    [(string-prefix? user-input "drop")(game-loop (drop-item (substring user-input 5 (string-length user-input)) game-state "true"))]
    [(string=? user-input "leave") (again? (check-leave-cond game-state))]
    [(string=? user-input "search") (game-loop (search-current-area game-state))]
    [(string=? user-input "help") (show-actions game-state)]
    [else (begin (displayln "Invalid command.") (help-menu))])  ; Give the user the help menu since they did not input a valid move
  (game-loop game-state))

; Tells the player the backstory
(backstory-text)
; Launches the game with an empty state
(game-loop default-game-state)
