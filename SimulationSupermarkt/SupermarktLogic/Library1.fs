module SupermarktLogic

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Input


type List<'a> = 
  | Empty 
  | Node of 'a * List<'a>
let (<<) x xs = Node(x,xs)

type SectionItem =
    {
        Position:    Vector2
        Category:    string
    }

type Item = 
    {
        Category:    string
        Price:       int
    }

type Customer = 
    {
        Position:    Vector2
        Bag:         List<Item>
        Velocity:    Vector2
        Money:       int
    }

type Register =
    {
        Position:    Vector2
        Cash:        int
    }

type GameState =
    {
        Customers:   List<Customer>
        Registers:   List<Register>
        SectionItems: List<SectionItem>
    }

