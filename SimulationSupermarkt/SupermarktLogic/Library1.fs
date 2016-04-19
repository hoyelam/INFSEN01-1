﻿module SupermarktLogic

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Input


//let (<<) x xs = Node(x,xs)

type Item = 
    | Beverage 
    | Bread
    | Vegetable 
    | Chips 
    | Candy

type Section =
    {
        Position1:   Vector2
        Position2:   Vector2
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
        Position1:   Vector2
        Position2:   Vector2
        Cash:        int
    }

type State =
    {
        KeyboardSpace:  bool;
    }

type GameState =
    {
        Customer:       Customer
        Register:       Register
        Sections:       List<Section>
        State   :       State
    }


let updateCustomerMoney (gameState: GameState) (item: Item) =
    {
        gameState.Customer with Money = gameState.Customer.Money - 100
    }


let initialState() = 
    {
        Register    = { Position1 = Vector2(663.0f, -30.0f); Position2 = Vector2(718.0f, 364.0f); Cash = 0 }
        State       = { KeyboardSpace = false }
        Sections    = [
                        { Position1 = Vector2(50.0f, 390.0f); Position2 = Vector2(507.0f, 430.0f);  Item  = Candy};
                        { Position1 = Vector2(158.0f, 257.0f); Position2 = Vector2(507.0f, 293.0f); Item  = Chips};
                        { Position1 = Vector2(0.0f, 15.0f); Position2 = Vector2(50.0f, 390.0f);     Item  = Beverage};
                        { Position1 = Vector2(158.0f, 117.0f); Position2 = Vector2(507.0f, 152.0f); Item  = Vegetable};
                        { Position1 = Vector2(50.0f, -30.0f); Position2 = Vector2(507.0f, 25.0f);   Item  = Bread};
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

let rec remove l predicate =
    match l with
    | [] -> []
    | x::rest -> rest



let Pay (item:Item) =

    item

let getFirstItem (l:List<Item>) :Item =
    let item = l |> Seq.head
    item

let getLastItem (l:List<Item>) :Item =
    let item = l |> Seq.last
    item

let getSubList (l:List<Item>) (length: int) : List<Item> =
    let subList =  l |> Seq.take length
    List.ofSeq subList

let getSpecificItem (l:List<Item>) (position: int) : Item = 
    let subList = getSubList l position
    let item = getLastItem subList
    item

let rec emptyBag (customer:Customer) =
    if customer.Bag.Length > 0 then
        let item = getFirstItem customer.Bag
        let customer = { 
            customer with Bag = remove customer.Bag (fun x -> item = item)
        }
        emptyBag customer
    else
        customer

let rec PayItem (customer:Customer) (count: int) : Customer =
    if customer.Bag.Length >= count then
        let item = getSpecificItem customer.Bag count
        let customer = 
            match item with 
                | Beverage ->   { 
                                    customer with Money = customer.Money - 5
                                }   
                | Bread ->      { 
                                    customer with Money = customer.Money - 10
                                }
                | Vegetable ->  { 
                                    customer with Money = customer.Money - 10
                                }
                | Chips ->      { 
                                    customer with Money = customer.Money - 5
                                }
                | Candy ->      { 
                                    customer with Money = customer.Money - 5
                                }

        PayItem customer (count + 1)
    else
        let customer = emptyBag customer
        customer   

let rec claimMoneys (register:Register) (customer:Customer) (count: int) : Register =
   
    if customer.Bag.Length >= count then
        let item = getSpecificItem customer.Bag count
        let register = 
            match item with 
                | Beverage ->   { 
                                    register with Cash = register.Cash + 5
                                }   
                | Bread ->      { 
                                    register with Cash = register.Cash + 10
                                }
                | Vegetable ->  { 
                                    register with Cash = register.Cash + 10
                                }
                | Chips ->      { 
                                    register with Cash = register.Cash + 5
                                }
                | Candy ->      { 
                                    register with Cash = register.Cash + 5
                                }

        claimMoneys register customer (count + 1)
    else        
        register   


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

let AddItem (customer: Customer) (gamestate:GameState) : Customer =
  let customer = gamestate.Customer

  // Candy
  let customer =
    if(customer.Position.X > (gamestate.Sections.Item(0).Position1.X - 20.0f) && customer.Position.X < (gamestate.Sections.Item(0).Position2.X + 20.0f)) && (customer.Position.Y > (gamestate.Sections.Item(0).Position1.Y - 20.0f) && customer.Position.Y < (gamestate.Sections.Item(0).Position2.Y + 20.0f)) then
      { 
        customer with Bag = List.append customer.Bag [gamestate.Sections.Item(0).Item]
      }
    else
      customer
  // Chips
  let customer =
    if(customer.Position.X > (gamestate.Sections.Item(1).Position1.X - 20.0f) && customer.Position.X < (gamestate.Sections.Item(1).Position2.X + 20.0f)) && (customer.Position.Y > (gamestate.Sections.Item(1).Position1.Y - 20.0f) && customer.Position.Y < (gamestate.Sections.Item(1).Position2.Y + 20.0f)) then
      { 
        customer with Bag = List.append customer.Bag [gamestate.Sections.Item(1).Item]
      }
    else
      customer
  // Beverages
  let customer =
    if (customer.Position.X > (gamestate.Sections.Item(2).Position1.X - 20.0f) && customer.Position.X < (gamestate.Sections.Item(2).Position2.X + 20.0f)) && (customer.Position.Y > (gamestate.Sections.Item(2).Position1.Y + 20.0f) && customer.Position.Y < (gamestate.Sections.Item(2).Position2.Y - 20.0f)) then
      { 
        customer with Bag = List.append customer.Bag [gamestate.Sections.Item(2).Item]
      }
    else
      customer
  // Vegetable
  let customer =
    if(customer.Position.X > (gamestate.Sections.Item(3).Position1.X - 20.0f) && customer.Position.X < (gamestate.Sections.Item(3).Position2.X + 20.0f)) && (customer.Position.Y > (gamestate.Sections.Item(3).Position1.Y - 20.0f) && customer.Position.Y < (gamestate.Sections.Item(3).Position2.Y + 20.0f)) then
      { 
        customer with Bag = List.append customer.Bag [gamestate.Sections.Item(3).Item]
      }
    else
      customer
  // Bread
  let customer =
    if(customer.Position.X > (gamestate.Sections.Item(4).Position1.X - 20.0f) && customer.Position.X < (gamestate.Sections.Item(4).Position2.X + 20.0f)) && (customer.Position.Y > (gamestate.Sections.Item(4).Position1.Y - 20.0f) && customer.Position.Y < (gamestate.Sections.Item(4).Position2.Y + 20.0f)) then
      { 
        customer with Bag = List.append customer.Bag [gamestate.Sections.Item(4).Item]
      }
    else
      customer
  customer

let CheckOut (customer: Customer) (gamestate:GameState) : Customer =
  let customer = gamestate.Customer
  let customer =
    if(customer.Position.X > (gamestate.Register.Position1.X - 20.0f) && customer.Position.X < (gamestate.Register.Position2.X + 20.0f)) && (customer.Position.Y > (gamestate.Register.Position1.Y - 20.0f) && customer.Position.Y < (gamestate.Register.Position2.Y + 20.0f)) then
      PayItem customer 1
    else
      customer
  customer

let updateCustomer (ks:KeyboardState) (ms:MouseState) (dt:float32) (gamestate:GameState) : Customer =
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
    if ks.IsKeyDown(Keys.Space) && (not gamestate.State.KeyboardSpace) then
      AddItem customer gamestate
    else
      customer

  let customer = 
    if ks.IsKeyDown(Keys.C) && (not gamestate.State.KeyboardSpace) then
      CheckOut customer gamestate
    else
      customer

  printfn "%A" customer.Bag
 

  let customer = 
    let newPos = customer.Position + customer.Velocity * dt
    if Collision newPos gamestate then
       {customer with Velocity = defaultVelocity}
    else
       {customer with Position = newPos
                      Velocity = customer.Velocity * 0.0f }
  customer


let updateState (ks:KeyboardState) (gamestate:GameState) : State =
  let state = gamestate.State

  let state =
    if ks.IsKeyDown(Keys.Space) then
      { 
        state with KeyboardSpace = true
      }
    else
      state
  let state = 
    if ks.IsKeyUp(Keys.Space) then
      { 
        state with KeyboardSpace = false
      }
    else
      state
  state


let updateRegister (ks:KeyboardState) (gamestate:GameState) : Register =
  let register = 
    if ks.IsKeyDown(Keys.C) && (not gamestate.State.KeyboardSpace) && (gamestate.Customer.Position.X > (gamestate.Register.Position1.X - 20.0f)) && (gamestate.Customer.Position.X < (gamestate.Register.Position2.X + 20.0f)) && (gamestate.Customer.Position.Y > (gamestate.Register.Position1.Y - 20.0f)) && (gamestate.Customer.Position.Y < (gamestate.Register.Position2.Y + 20.0f)) then
        claimMoneys gamestate.Register gamestate.Customer 1
    else
        gamestate.Register

  register


let updateGameState (ks:KeyboardState) (ms:MouseState) (dt:float32) (gameState:GameState) =
    {
        gameState with Register = updateRegister ks gameState
                       Customer = updateCustomer ks ms dt gameState
                       State    = updateState ks gameState
                       
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