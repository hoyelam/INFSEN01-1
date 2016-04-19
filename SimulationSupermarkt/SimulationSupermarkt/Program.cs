using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Microsoft.Xna.Framework;
using Microsoft.Xna.Framework.Input;
using Microsoft.Xna.Framework.Graphics;


namespace SimulationSupermarkt
{
    class Game : Microsoft.Xna.Framework.Game
    {
        SpriteBatch spriteBatch;
        GraphicsDeviceManager graphics;
        SupermarktLogic.GameState gameState;

        public Game()
        {
            graphics = new GraphicsDeviceManager(this);
            Content.RootDirectory = "Content";
        }

        protected override void LoadContent()
        {
            spriteBatch = new SpriteBatch(GraphicsDevice);
            gameState = SupermarktLogic.initialState();
            base.LoadContent();
        }

        protected override void Update(GameTime gameTime)
        {
            gameState = SupermarktLogic.updateGameState(Keyboard.GetState(), Mouse.GetState(), (float)gameTime.ElapsedGameTime.TotalSeconds,
            gameState);

        

            base.Update(gameTime);
        }

        protected override void Draw(GameTime gameTime)
        {
            //GraphicsDevice.Clear(Color.CornflowerBlue);
            // Load the background content.
            Texture2D background;
            background = Content.Load<Texture2D>("background.png");

            // Set the rectangle parameters.
            Rectangle mainFrame;
            mainFrame = new Rectangle(0, 0, GraphicsDevice.Viewport.Width, GraphicsDevice.Viewport.Height);

            spriteBatch.Begin();

            spriteBatch.Draw(background,mainFrame,Color.White);
            foreach (var drawable in SupermarktLogic.drawState(gameState))
            {
                spriteBatch.Draw(Content.Load<Texture2D>(drawable.Image),
                drawable.Position, Color.White);
            }

            SpriteFont font = Content.Load<SpriteFont>("Font");
            spriteBatch.DrawString(font, "Money:" + gameState.Customer.Money, new Vector2(20.0f, 445.0f), Color.Black);
            spriteBatch.DrawString(font, "Inventory: " + gameState.Customer.Bag.Length, new Vector2(240.0f, 445.0f), Color.Black);
            spriteBatch.DrawString(font, "Store: " + gameState.Register.Cash, new Vector2(660.0f, 445.0f), Color.Black);

            spriteBatch.End();

            base.Draw(gameTime);
        }
    }

    class Program
    {
        static void Main(string[] args)
        {
            using (var game = new Game())
            {
                game.Run();
            }
        }
    }
}