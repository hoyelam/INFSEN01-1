module SupermarktLogic

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Input

//let (<<) x xs = Node(x,xs)

type Item = 
    {
        Category:    string
        Price:       int
    }

type Section =
    {
        Position1:   Vector2
        Position2:   Vector2
        Category:    string
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
        Position1:   Vector2
        Position2:   Vector2
        Cash:        int
    }

type GameState =
    {
        Customer:       Customer
        Registers:      List<Register>
        Sections:       List<Section>
    }

let initialState() = 
  {
    Registers   = [
                    { Position1 = Vector2(200.0f, 200.0f); Position2 = Vector2(300.0f, 300.0f); Cash = 100}
    ]
    Sections    = [
                    { Position1 = Vector2(300.0f, 200.0f); Position2 = Vector2(600.0f, 400.0f); Category  = "Candy"};
                    { Position1 = Vector2(200.0f, 200.0f); Position2 = Vector2(300.0f, 300.0f); Category  = "Chips"};
                    { Position1 = Vector2(200.0f, 200.0f); Position2 = Vector2(300.0f, 300.0f); Category  = "Beverages"};
                    { Position1 = Vector2(200.0f, 200.0f); Position2 = Vector2(300.0f, 300.0f); Category  = "Fruit"};
                    { Position1 = Vector2(200.0f, 200.0f); Position2 = Vector2(300.0f, 300.0f); Category  = "Bread"};
    ] 
    Customer    = 
      {
        Position    = Vector2(590.0f, 400.0f)
        Velocity    = Vector2.Zero
        Bag         = []
        Money       = 100
        Image       = "up.png"
      }
  }

let checkSections (newPos:Vector2) (gamestate:GameState) : bool =
    let found = false

    // traverse all sections
//    for section in gamestate.Sections do     
//        if (newPos.X > section.Position1.X && newPos.X < section.Position2.X) && (newPos.Y > section.Position1.Y && newPos.Y < section.Position2.Y) then
//            found = true
                
    // if newPos is on a section
    if found then
        true

    // no collisions
    else
        false

let moveCustomer (ks:KeyboardState) (ms:MouseState) (dt:float32) (gamestate:GameState) : Customer =
  let speed = 8000.0f
  let customer = gamestate.Customer
  let defaultVelocity = customer.Velocity
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
    if checkSections newPos gamestate then
       {customer with Velocity = defaultVelocity}
    else
       {customer with Position = newPos
                      Velocity = customer.Velocity * 0.0f }

  customer

let updateState (ks:KeyboardState) (ms:MouseState) (dt:float32) (gameState:GameState) =
    {
        gameState with Customer = moveCustomer ks ms dt gameState
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