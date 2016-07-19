/*
    This file is part of ProyectoCO.

    ProyectoCO is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    ProyectoCOis distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with ProyectoCO.  If not, see <http://www.gnu.org/licenses/>.
 
   @authors
   Andres Humberto Agredo Bermudez 0623078 andres.humberto.agredo@correounivalle.edu.co
   Fernando Sanchez Quintero 1225394 
   Juan Diego Prado Ramos 1226218 
   Nelson Portilla 1226934
 */

package proyectoco;

import java.util.Vector;
import java.util.logging.Level;
import java.util.logging.Logger;
import lpsolve.LpSolve;
import lpsolve.LpSolveException;

public class Solver {
    public LpSolve solver;
    private int meses;
    private int [] temperaturas;
    private int [] precipitacion;
    private int [] d_min;
    private int [] d_max;
    private int produccion;
    private int precio;
    private long TInicio, TFin, tiempo; //Variables para determinar el tiempo de ejecución
    
//metodo que inicaliza el solver 
    public Solver(int meses, Vector T, Vector Pr, Vector Dmin, Vector Dmax, int X, int $C) {
        try {             
            this.meses = meses;
            this.produccion = X;
            this.precio = $C;
            
            // 0: numero restricciones inicialmente, meses: cantidad de varibles del modelo
            this.solver= LpSolve.makeLp(0, this.meses);
            //vuelve binarias las variables
            for (int s =1 ; s<=this.meses;s++){
                this.solver.setBinary(s, true);
            }
            
            //asignacion del tamaño de los arreglos al numero de meses
            this.temperaturas = new int [meses];
            this.precipitacion = new int [meses];
            this.d_min = new int [meses];
            this.d_max = new int [meses];      
            
            //Asignacion de valores
            
            for (int i =0; i<T.size(); i++){
                this.temperaturas[i] = Integer.parseInt((String) T.elementAt(i));
            }  
            
            for (int i =0; i<Pr.size(); i++){
                this.precipitacion[i] = Integer.parseInt((String) Pr.elementAt(i));
            }  
            
            for (int i =0; i<Dmin.size(); i++){
                this.d_min[i] = Integer.parseInt((String) Dmin.elementAt(i));
            }            
            
            for (int i =0; i<Dmax.size(); i++){
                this.d_max[i] = Integer.parseInt((String) Dmax.elementAt(i));
            }            
            
            System.out.println("inicialized");
        } catch (LpSolveException ex) {
            Logger.getLogger(Solver.class.getName()).log(Level.SEVERE, null, ex);
        }
        
    }  
    
    //calcula los coeficientes de la funcion objteivo y la pasa al solver
    public void funcion_obj () 
    {
        
        int prod = this.produccion;
        int valor = this.precio;
        double  []  coeficientes = new double [this.meses];
        // en los 3 primeros meses no se puede cosechas
        coeficientes [0] = 0;
        coeficientes [1] = 0;
        coeficientes [2] = 0;
        double coeficiente;
        for (int i = 3; i< this.meses;i++)
        {
            //si la produccion esta dentro del rango de la demanda
            if ((prod >= this.d_min[i]) && (prod  <= this.d_max[i]))
            {
                coeficiente = valor*prod;
            }
            else
            {
                //si la produccion es menor que la demanda minina
                if (prod < this.d_min[i])
                {
                    coeficiente = (valor /2) * prod;
                }
                //si la produccion es mayor que la demanda maxima   
                else
                {
                    coeficiente = (valor * this.d_max[i]) + (produccion - this.d_max[i]) * (valor/2); 
                }
            }
            coeficientes [i] = coeficiente; 
        }
        
        String f_obj = "";
        for (int a = 0; a<this.meses;a++)
            {
                //System.out.println(coeficientes[a]);
                f_obj = f_obj + "-" + coeficientes[a] + " ";
            }
        try {
            // se le pasa la funcion objetivo al solver
            solver.strSetObjFn(f_obj);
            System.out.println("fun_obj añadida");
        } catch (LpSolveException ex) {
            System.err.println("error en set function");
        }
        
    }
    
    //todas las restricciones del modelo
    public void restricciones ()
    {
        //añade restricciones para poder cultivar
        cond_cultivar ();
        //restriccion para que en los primeros 3 meses no se coseche
        no_cosechar();
        //restringir en 4 meses a lo maximo 1 sola cosecha
        restr_cosecha ();
        
    }
    
    //restriccion de que en los primeros 3 meses no se coseche
    public void no_cosechar ()
    {
        //x1 <=0
        //x2 <=0
        //x3 <=0
        try {
            String r1;
            r1 = "1 1 1";
            for (int i=0 ; i<this.meses - 3;i++)
            {
                r1 = r1 + " 0";
            }
            //System.out.println(r1);
            solver.strAddConstraint(r1, LpSolve.EQ, 0);
            System.out.println("R no cosechar");
            
        } catch (LpSolveException ex) {
            Logger.getLogger(Solver.class.getName()).log(Level.SEVERE, null, ex);
        }
    }
    
    //restriccion de que un periodo de 4 meses solo exista una cosecha
    public void restr_cosecha ()
    {
        int [] r = new int [meses];
        for (int a =0; a<meses;a++)
        {
           r[a] = 0;
        }
        for (int i =0; i<meses-3;i++)
        {
            try {
                //añadir restriccion xi + x(i+1) + x(i+2) + x(i+3) <=1
                r[i] = 1;
                r[i+1] = 1;
                r[i+2] = 1;
                r[i+3] = 1;
                String constr = "";
                for (int a = 0; a<this.meses;a++)
                {
                    //System.out.println(coeficientes[a]);
                    constr = constr + r[a] + " ";
                }
                solver.strAddConstraint(constr, LpSolve.LE, 1);
                
                
                r[i] = 0;
                r[i+1] = 0;
                r[i+2] = 0;
                r[i+3] = 0;
            } catch (LpSolveException ex) {
                Logger.getLogger(Solver.class.getName()).log(Level.SEVERE, null, ex);
            }
            
        }
        System.out.println("rest_cosecha");
    }
    
    //verifica si en el mes i se puede cosechar
    public void cond_cultivar () 
    {
        int meses = this.meses;
        int [] temperaturas = this.temperaturas;
        int [] precipitacion = this.precipitacion;
        int [] r = new int [meses];
        for (int a =0; a<meses;a++)
        {
           r[a] = 0;
        }
        for (int i =0; i<meses-3;i++)
        {
            if (!condiciones (temperaturas[i],precipitacion[i]))
            {
                //añadir restriccion que x(i+3) =0
                r[i+3]=1;
            }
            String restriccion = "";
            for (int a = 0; a<this.meses;a++)
            {
                //System.out.println(coeficientes[a]);
                restriccion = restriccion + r[a] + " ";
                r[a] = 0;
            }
            try {
                //System.out.println(restriccion);
                this.solver.strAddConstraint(restriccion, LpSolve.EQ, 0);
            } catch (LpSolveException ex) {
                Logger.getLogger(Solver.class.getName()).log(Level.SEVERE, null, ex);
            }
        }
        System.out.println("rest_cond cultivar");
    }
    
    //verifica si en el mes i se puede cosechar
    public boolean condiciones (int temperatura, int precip)
    {
        boolean flag = false;
        if ((temperatura >= 18 ) && (temperatura <=20) && (precip >=63))
        {
            flag = true;
        }
        return flag;
    }
    
    //ejecuta el solver
    public void ejecutar ()
    {
        try {
            TInicio = System.currentTimeMillis(); //Tomamos la hora en que inicio el solver
            this.solver.solve(); 
            TFin = System.currentTimeMillis(); //Tomamos la hora en que finalizó el solver
            tiempo = TFin - TInicio; //Calculamos los milisegundos de diferencia
            //Mostramos en pantalla el tiempo de ejecución en milisegundos
            System.out.println("Tiempo de ejecución es: "+tiempo+" milisegundos"); 
        } catch (LpSolveException ex) {
            Logger.getLogger(Solver.class.getName()).log(Level.SEVERE, null, ex);
        }
    }
    
}
