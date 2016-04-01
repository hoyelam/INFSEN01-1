module SupermarktLogic

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Input

type List<'a> = 
  | Empty 
  | Node of 'a * List<'a>
let (<<) x xs = Node(x,xs)

type Item = 
    {
        Category:    string
        Price:       int
    }

type SectionItem =
    {
        Position:    Vector2
        Category:    string
        Item:        Item
    }

type Customer = 
    {
        Position:    Vector2
        Bag:         List<Item>
        Velocity:    Vector2
        Money:       int
        Image:       string
    }

type Register =
    {
        Position:    Vector2
        Cash:        int
    }

type GameState =
    {
        Customer:       Customer
        Registers:      List<Register>
        SectionItems:   List<SectionItem>
    }

let initialState() = 
  {
    Registers   = Empty
    SectionItems = Empty
    Customer = 
      {
        Position = Vector2(320.0f, 400.0f)
        Velocity = Vector2.Zero
        Bag = Empty
        Money = 100
        Image = "up.png"
      }
  }

let checkBorder (newPos:Vector2) : bool =
    // left wall
    if newPos.X < 50.0f then
        true
    else
        false

let moveCustomer (ks:KeyboardState) (ms:MouseState) (dt:float32) (customer:Customer) : Customer =
  let speed = 8000.0f;
  let defaultVelocity = customer.Velocity;
  let customer =
    if ks.IsKeyDown(Keys.Left) then
      { customer with Velocity = customer.Velocity - Vector2.UnitX * speed * dt
                      Image    = "left.png"
      }
    else
      customer
  let customer = 
    if ks.IsKeyDown(Keys.Right) then
      { customer with Velocity = customer.Velocity + Vector2.UnitX * speed * dt 
                      Image    = "right.png"
      }
    else
      customer
  let customer =
    if ks.IsKeyDown(Keys.Down) then
      { customer with Velocity = customer.Velocity + Vector2.UnitY * speed * dt 
                      Image    = "down.png"
      }
    else
      customer
  let customer = 
    if ks.IsKeyDown(Keys.Up) then
      { customer with Velocity = customer.Velocity - Vector2.UnitY * speed * dt 
                      Image    = "up.png"
      }
    else
      customer

  let customer = 
    let newPos = customer.Position + customer.Velocity * dt
    if checkBorder newPos then
       {customer with Velocity = defaultVelocity}
    else
       {customer with Position = newPos
                      Velocity = customer.Velocity * 0.0f }

  customer

let updateState (ks:KeyboardState) (ms:MouseState) (dt:float32) (gameState:GameState) =
    {
        gameState with Customer = moveCustomer ks ms dt gameState.Customer
    } 
     

type Drawable =
    {
        Position: Vector2
        Image:    string
    }

let drawState (gameState: GameState) : seq<Drawable> =
    [
        {
            Drawable.Position = gameState.Customer.Position
            Drawable.Image    = gameState.Customer.Image
        }
    ] |> Seq.ofList