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
        Register:       Register
        Sections:       List<Section>
    }

let initialState() = 
  {
    Register   = { Position1 = Vector2(663.0f, -30.0f); Position2 = Vector2(718.0f, 364.0f); Cash = 100 }
    Sections    = [
                    { Position1 = Vector2(50.0f, 390.0f); Position2 = Vector2(507.0f, 430.0f);  Category  = "Candy"};
                    { Position1 = Vector2(158.0f, 257.0f); Position2 = Vector2(507.0f, 293.0f); Category  = "Chips"};
                    { Position1 = Vector2(0.0f, 15.0f); Position2 = Vector2(50.0f, 390.0f);     Category  = "Beverages"};
                    { Position1 = Vector2(158.0f, 117.0f); Position2 = Vector2(507.0f, 152.0f); Category  = "Fruit"};
                    { Position1 = Vector2(50.0f, -30.0f); Position2 = Vector2(507.0f, 25.0f);   Category  = "Bread"};
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

let Collision (newPos:Vector2) (gamestate:GameState) : bool =
    let mutable collision = false

    // check world borders
    if newPos.X <= 0.0f then
        collision <- true
    else if newPos.Y <= 0.0f then
        collision <- true
    else if newPos.X >= 712.0f then
        collision <- true
    else if newPos.Y >= 428.0f then
        collision <- true

    // check all sections
    for section in gamestate.Sections do      
        if (newPos.X > section.Position1.X && newPos.X < section.Position2.X) && (newPos.Y > section.Position1.Y && newPos.Y < section.Position2.Y) then
           collision <- true

    // check kassa
    if (newPos.X > gamestate.Register.Position1.X && newPos.X < gamestate.Register.Position2.X) && (newPos.Y > gamestate.Register.Position1.Y && newPos.Y < gamestate.Register.Position2.Y) then
        collision <- true
              
    collision

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
//      printfn "%A %A" customer.Position.X customer.Position.Y
      { customer with Velocity = customer.Velocity - Vector2.UnitY * speed * dt 
                      Image    = "up.png"
      }
    else
      customer

  let customer = 
    let newPos = customer.Position + customer.Velocity * dt
    if Collision newPos gamestate then
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