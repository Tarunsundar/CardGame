module CardGames (makePack,beats,deal,Pack,Hand)

where

data Suit = Clubs | Diamonds | Hearts | Spades
                      deriving (Eq, Ord, Show)

data Value = Number Int | Jack | Queen | King
                      deriving (Eq, Ord)

data PlayingCard = Card Value Suit


-- Some example cards, try defining others.

myCard    = Card (Number 9) Diamonds
yourCard  = Card Queen Hearts
firstCard = Card (Number 2) Clubs
aceOfDiamonds = Card (Number 1) Diamonds


-- Define our own Show instances here for elements of 
-- the Value and PlayingCard types 

instance Show Value where
  show (Number 1) = "Ace"
  show (Number n) = show n
  show Jack       = "Jack"
  show Queen      = "Queen"
  show King       = "King"

instance Show PlayingCard where
  show (Card value suit) = 
     show value ++ " of " ++ show suit



-- A Pack and a Hand are both just lists of PlayingCards

type Pack = [PlayingCard]
type Hand = [PlayingCard]



---------------------------------------------------------
-- beats tells us if one card has a higher value than another

beats :: PlayingCard -> PlayingCard -> Bool
beats (Card value1 suit1) (Card value2 suit2) =
   suit1 == suit2 && value1 > value2 



---------------------------------------------------------
-- makePack will create the full pack of cards 
-- using a list comprehension


makePack :: Pack
makePack = [Card y z | y <- makeCardValues, z <- [Clubs, Diamonds, Hearts, Spades] ]


-- makeCardValues will make a list of all the possible values 
-- from Ace (which is Number 1) up to King

makeCardValues :: [Value]
makeCardValues = [ Number x | x <- [1..10] ] ++ [Jack, Queen, King]



---------------------------------------------------------
-- deal will give you the top n cards from a pack, 
-- and the remainder of the pack

deal :: Int -> Pack -> (Hand, Pack)
deal n pack = (hand, rest)
  where
   hand = take n pack
   rest = drop n pack

---------------------------------------------------------
-- okValue will return the value if the suit of the 
-- playing card matches the one that is given 
-- and Nothing if it doesn't.

okValue :: Suit -> PlayingCard -> Maybe Value
okValue s (Card v suit) = 
  if s == suit 
    then Just v
    else Nothing 
 
  

 
