# CS441-Assignment4

##Grocery Nightmare

This game takes place inside a grocery store.
There are 4 locations in this store, the Checkout, the Bakery, the Fridge, and the Freezer.
You, the player, spawn in the Checkout.
There are 5 total inventory items available for picking up and dropping.
The Bread and Sugar are located in the Bakery.
The Milk and Eggs are located in the Fridge.
The Ice Cream is located in the Freezer, but can only be found after searching the area.
The goal is to get at least the Milk, Bread, Eggs, and Sugar then go to the checkout and leave.
The player can also get the Ice Cream and leave with that for a secret alternative ending.
You CAN NOT move S from the fridge, since there are two possible paths you MUST specify SW or SE.
All commands are case-insensitive.

Optional features included:
Multiple endings - Different endings based on if you picked up the ice cream or not
Variable descriptions - Each time you enter a new area there is a long description, and each subsequent time after there is a short description. You can repeat the long description by using the "where am I?" Command.

Sources used for this project:
Racket Reference: Used for looking up functions and how to use them
Claude AI: Asked to generate simple functions such as "How to take input from the user?" and "What function can be used to check if an item is in a list?" A full list of prompts are listed below.

How to take input from the user?
What function can be used to check if an item is in a list?
How to loop through all items in a list in racket
What function can be used to remove prefixes of words in Racket
Write a function in Racket to take a list and print out the items on the list in the middle of a sentence
Write a simple function to trim input in racket
how to stop the program in racket
Follow up: is there another way?
how to check if a string is equal to many things in one line in racket
how to check if #t or #f
Write an example of writing to the Racket terminal with colored text
how to apply string-dowcase to a list of sublists in racket
Give me a function to loop through each item in a list and check it against a condition
