# anapioficeandfire-haskell [![Build Status](https://travis-ci.org/kberger/anapioficeandfire-haskell.svg?branch=master)](https://travis-ci.org/kberger/anapioficeandfire-haskell)
A Haskell wrapper for interaction with www.anapioficeandfire.com

## Usage Example
```haskell
> jonSnow <- getCharacterById 583
> jonSnow
Just (Character {charUrl = "http://www.anapioficeandfire.com/api/characters/583", 
                 charName = "Jon Snow", 
                 gender = "Male", 
                 culture = "Northmen", 
                 born = "In 283 AC", 
                 died = "", 
                 charTitles = ["Lord Commander of the Night's Watch"], 
                 aliases = ["Lord Snow",
                            "Ned Stark's Bastard",
                            "The Snow of Winterfell",
                            "The Crow-Come-Over",
                            "The 998th Lord Commander of the Night's Watch",
                            "The Bastard of Winterfell",
                            "The Black Bastard of the Wall",
                            "Lord Crow"], 
                 father = "", 
                 mother = "", 
                 spouse = "", 
                 allegiances = ["http://www.anapioficeandfire.com/api/houses/362"], 
                 books = ["http://www.anapioficeandfire.com/api/books/5"], 
                 povBooks = ["http://www.anapioficeandfire.com/api/books/1",
                             "http://www.anapioficeandfire.com/api/books/2",
                             "http://www.anapioficeandfire.com/api/books/3",
                             "http://www.anapioficeandfire.com/api/books/8"], 
                 tvSeries = ["Season 1",
                             "Season 2",
                             "Season 3",
                             "Season 4",
                             "Season 5",
                             "Season 6"], 
                 playedBy = ["Kit Harington"]
                }
     )
```
