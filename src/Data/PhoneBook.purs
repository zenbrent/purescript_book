module Data.Phonebook where

import Data.List
import Data.Maybe

-- The Control.Plus module defines the empty value, which we will need later.
-- Notice that the imports for this module are listed explicitly in parentheses.
-- This is generally a good practice, as it helps to avoid conflicting imports.
import Control.Plus (empty)

-- define a function with a header
{--
add :: Number -> Number -> Number
add x y = x + y
--}

{--
-- define a function inline
-- This works in psci, won't compile here though.
let
    add :: Number -> Number -> Number
    add = \x y -> x + y
--}

{--
-- flip has a universally qualified type -- i.e. it takes 3 arguments
-- that are of any type.
flip (\n s -> show n ++ "," ++ s) "Ten" 10

-- However, the types do have to be consistent. e.g. this doesn't work:
flip 10 "Ten" (\n s -> show n ++ "," ++ s)
--}


{--
-- psc is whitespace sensitive!
-- this works:
add x y x = x +
    y + x

-- this doesn't:
add x y x = x +
y + x
--}

-- Define a "type synonym" for Entry: the type Entry is equivalent to the type on the right.
type Entry = { firstName::String, lastName :: String, phone :: String }

-- List is a "type constructor". Things don't have the type List, List takes a type argument
-- and creates a new type.
type PhoneBook = List Entry

-- Values are distinguished by their types.
   -- To check the type of a type in psci, use :t
-- Types are distinguished by their kinds. * is the kind of all types that have values.
   -- To check the kind of a type in psci, use :k

-- Declare the type of a showEntry function:
showEntry :: Entry -> String

-- and define the function
showEntry entry = entry.lastName ++ ", " ++
                  entry.firstName ++ ", " ++
                  entry.phone

emptyBook :: PhoneBook
emptyBook = empty

-- Take Entry as the first arg, PhoneBook as the second, and return PhoneBook
-- Functions actually only take 1 arg, and are right-assiciatively curried:
-- Entry -> (PhoneBook -> PhoneBook)
insertEntry :: Entry -> PhoneBook -> PhoneBook
{-- The original version:
insertEntry entry book = Cons entry book
-- but because Cons entry book is actually 2 function calls, and the last arg is the same,
-- adding it to the function is redundant:
insertEntry entry = Cons entry
-- Also, entry is redundant:
--}
insertEntry = Cons
-- This is called "eta conversion", and along with other techniques, can be used to convert functions
-- to "point free" form -- i.e. without reference to their arguments.
-- It is arguable whether point-free form is better in general.

findEntry :: String -> String -> PhoneBook -> Maybe Entry

-- Bring firstName, lastName, and book into scope
-- findEntry is the composition of a filtering function and the head function
-- also, note that $ is the infix operator. It takes a function and a value and applies the fn to the val.
    -- forall a b. (a -> b) -> a -> b
-- But why would we need to use $ instead of regular function application? The reason is that $ is a right-associative, low precedence operator. This means that $ allows us to remove sets of parentheses for deeply- nested applications.
    -- e.g. street (address (boss employee)) becomes street $ address $ boss employee

{-- Originally:
findEntry firstName lastName book = head $ filter filterEntry book
-- Using the composition operator: --}
findEntry firstName lastName book = (head <<< filter filterEntry) book
  where
      -- filterEntry is defined as an auxiliary declaration
      filterEntry :: Entry -> Boolean
      filterEntry entry = entry.firstName == firstName && entry.lastName == lastName

{--
-- or it could be
findEntry firstName lastName book =  filter >>> filterEntry head  book
--}
{-- Exercise 3.1:
Test your understanding of the findEntry function by writing down the types of each of its major subexpressions. For example, the type of the head function as used is specialized to List Entry -> Maybe Entry.
<<< ??
filter
    me:
    (Entry -> Boolean) -> List Entry -> List Entry
    actual:
    forall a. (a -> Prim.Boolean) -> Data.List.List a -> Data.List.List a

==
    me:
    Entry -> Entry -> Boolean
    actual:
    forall a. (Prelude.Eq a) => a -> a -> Prim.Boolean

&&
    me:
    Boolean -> Boolean -> Boolean
--}



-- <$> can lift a function over a particular type constructor like Maybe.
{--
let printEntry firstName lastName book = showEntry <$> findEntry firstName lastName book
--}

{-- Exercise 3.2:
Write a function which looks up an Entry given a phone number, by reusing the existing code in findEntry. Test your function in psci.
--}

findEntryByPhone :: String -> PhoneBook -> Maybe Entry
findEntryByPhone phone book = (head <<< filter filterEntry) book
    where
        filterEntry :: Entry -> Boolean
        filterEntry entry = entry.phone == phone

{-- psci entry:
let brent = { firstName: "brent", lastName: "brimhall", phone: "555-123-4567" }
let courtney = { firstName: "c", lastName: "b", phone: "585-123-4567" }
let book1 = insertEntry courtney (insertEntry brent emptyBook)
let printEntry phone book = showEntry <$> findEntryByPhone phone book
printEntry "555-123-4567" book1
--}


-- TODO: These exercises
{-- Exercise 3.3, moderate difficulty:
Write a function which tests whether a name appears in a PhoneBook, returning a Boolean value. Hint: Use psci to find the type of the Data.List.null function, which test whether a list is empty or not.
--}

{-- Exercise 3.4, moderate difficulty:
Write a function removeDuplicates which removes duplicate phone book entries with the same first and last names. Hint: Use psci to find the type of the Data.List.nubBy function, which removes duplicate elements from a list based on an equality predicate.
--}
