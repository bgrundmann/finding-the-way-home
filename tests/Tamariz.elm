module Tamariz exposing (tamariz)


tamariz : String
tamariz =
    """def deal a b
    doc Deal 1 card from a to b
    cut 1 a b
end
def deal N a b
  doc Deal N cards from a to b
  repeat N
     deal a b
  end
end

def reverse N pile 
   doc Reverse the top N cards of pile
   temp t
   deal N pile t
   cut N t pile
end

def completecut N deck
   doc Cut the top N cards of deck to the bottom
   temp t
   cut N deck t
   turnover deck
   turnover t
   cut N t deck
   turnover deck
end

def outfaro N deck pile
   doc Out-faro pile into deck
   temp t tr
   repeat N 
      deal 1 pile t
      deal 1 deck t
   end
   deal N t tr
   deal N t tr
   cut N tr t
   cut N tr deck
   cut N t deck
end

def infaro N deck pile
   doc In-faro pile into deck
   temp t tr
   repeat N 
      deal 1 deck t
      deal 1 pile t
   end
   deal N t tr
   deal N t tr
   cut N tr t
   cut N tr deck
   cut N t deck
end

def mnemonicaFromUSPC deck
  doc Rearrange deck (in USPC) new deck order into mnemonica
  temp upper hand
  
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
