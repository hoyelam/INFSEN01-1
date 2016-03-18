using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using System.Windows.Forms;
using FunctionalFunctions;

namespace SimulationSupermarkt
{
    static class Program
    {
        /// <summary>
        /// The main entry point for the application.
        /// </summary>
        [STAThread]
        static void Main()
        {
            Application.EnableVisualStyles();
            Application.SetCompatibleTextRenderingDefault(false);
            Application.Run(new Form1());
            Persoon p1 = new Persoon();
            setDefaultsPerson(p1);

        }

        class Persoon
        {
            public int budget { get; set; }
            public Product[] product { get; set; }
            public int movement { get; set; }
        }

        class Product
        {
            public string naam { get; set; }
            public int prijs { get; set; }
        }

        class Store
        {
            public double revenue { get; set; }
        }

        class Point
        {
            public int x { get; set; }
            public int y { get; set; }
            public Product[] itemVak { get; set; }
            public int id { get; set; }
        }

        /// <summary>
        /// spawn 
        /// </summary>
        /// <param name="customer"></param>
        public static void setDefaultsPerson(Persoon customer)
        {
            customer.budget = 100;
            customer.movement = 5;
        }
    }
}
