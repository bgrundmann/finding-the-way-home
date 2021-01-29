module Tamariz exposing (tamariz)


tamariz : String
tamariz =
    """def deal N a b
  doc Deal N cards from a to b
  repeat N
     cut 1 a b
  end
end

def reverse N pile 
   doc Reverse the top N cards of pile
   deal N pile temp
   cut N temp pile
end

def completecut N deck
   doc Cut the top N cards of deck to the bottom
   cut N deck temp
   turnover deck
   turnover temp
   cut N temp deck
   turnover deck
end

def outfaro N deck pile
   doc Out-faro pile into deck
   repeat N 
      deal 1 pile temp
      deal 1 deck temp
   end
   deal N temp tempreversed
   deal N temp tempreversed
   cut N tempreversed temp
   cut N tempreversed deck
   cut N temp deck
end

def infaro N deck pile
   doc In-faro pile into deck
   repeat N 
      deal 1 deck temp
      deal 1 pile temp
   end
   deal N temp tempreversed
   deal N temp tempreversed
   cut N tempreversed temp
   cut N tempreversed deck
   cut N temp deck
end

def mnemonicaFromUSPC deck
  doc Rearrange deck (in USPC) new deck order into mnemonica
  
  repeat 4
    cut 26 deck upper
    outfaro 26 deck upper
  end 
  reverse 26 deck
  completecut 26 deck  
  cut 18 deck hand
    outfaro 18 deck hand
    completecut 9 deck
end

mnemonicaFromUSPC deck
"""
