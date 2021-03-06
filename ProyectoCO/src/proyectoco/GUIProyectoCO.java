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

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.Collection;
import java.util.StringTokenizer;
import java.util.Vector;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.swing.text.DefaultEditorKit;
import lpsolve.LpSolveException;

public class GUIProyectoCO extends javax.swing.JFrame {

    /**
     * Creates new form GUIProyectoCO
     */
    public GUIProyectoCO() {
        initComponents();
    }

    /**
     * This method is called from within the constructor to initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is always
     * regenerated by the Form Editor.
     */
    @SuppressWarnings("unchecked")
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        jFileChooser1 = new javax.swing.JFileChooser();
        jFileChooser2 = new javax.swing.JFileChooser();
        jScrollPane1 = new javax.swing.JScrollPane();
        jTextArea1 = new javax.swing.JTextArea();
        jMenuBar1 = new javax.swing.JMenuBar();
        jMenuFile = new javax.swing.JMenu();
        jMenuItemOpen = new javax.swing.JMenuItem();
        jMenuItemSave = new javax.swing.JMenuItem();
        jMenuItemExit = new javax.swing.JMenuItem();

        jFileChooser1.setDialogTitle("Abrir Archivo");
        jFileChooser1.setFileFilter(new MyCustomFilter());

        jFileChooser2.setFileFilter(new MyCustomFilter());

        setDefaultCloseOperation(javax.swing.WindowConstants.EXIT_ON_CLOSE);

        jTextArea1.setColumns(20);
        jTextArea1.setRows(5);
        jScrollPane1.setViewportView(jTextArea1);

        jMenuFile.setText("Archivo");

        jMenuItemOpen.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_A, java.awt.event.InputEvent.ALT_MASK));
        jMenuItemOpen.setText("Abrir");
        jMenuItemOpen.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jMenuItemOpenActionPerformed(evt);
            }
        });
        jMenuFile.add(jMenuItemOpen);

        jMenuItemSave.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_G, java.awt.event.InputEvent.CTRL_MASK));
        jMenuItemSave.setText("Guardar");
        jMenuItemSave.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jMenuItemSaveActionPerformed(evt);
            }
        });
        jMenuFile.add(jMenuItemSave);

        jMenuItemExit.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_Q, java.awt.event.InputEvent.ALT_MASK));
        jMenuItemExit.setText("Salir");
        jMenuItemExit.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jMenuItemExitActionPerformed(evt);
            }
        });
        jMenuFile.add(jMenuItemExit);

        jMenuBar1.add(jMenuFile);

        setJMenuBar(jMenuBar1);

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(getContentPane());
        getContentPane().setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addComponent(jScrollPane1, javax.swing.GroupLayout.DEFAULT_SIZE, 400, Short.MAX_VALUE)
        );
        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addComponent(jScrollPane1, javax.swing.GroupLayout.DEFAULT_SIZE, 279, Short.MAX_VALUE)
        );

        pack();
    }// </editor-fold>//GEN-END:initComponents

    private void jMenuItemOpenActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jMenuItemOpenActionPerformed
        int returnVal = jFileChooser1.showOpenDialog(this);
        if (returnVal == jFileChooser1.APPROVE_OPTION) {
            File file = jFileChooser1.getSelectedFile();
            try {  
                jTextArea1.setText(null);
                Builder(file);
                Solver solv = new Solver(this.K, this.T, this.Pr, this.Dmin, this.Dmax, this.X, this.$C);
                solv.funcion_obj();
                solv.restricciones();
                solv.ejecutar();
                double[] var = solv.solver.getPtrVariables();                
                jTextArea1.append(""+ ((int) solv.solver.getObjective() * -1));                
                
                //Contar la cantidad de soluciones:
                int j = 0;
                for (int i = 0; i < var.length; i++) {
                    if(var[i]==1)
                        j++;
                }                
                jTextArea1.append("\r\n"+j);
                for (int i = 0; i < var.length; i++) {
                    if(var[i]==1)
                        jTextArea1.append("\r\n"+(i+1));
                }                
                
            } catch (IOException ex) {
                System.out.println("problem accessing file"+file.getAbsolutePath());
            } catch (LpSolveException ex) {
                Logger.getLogger(GUIProyectoCO.class.getName()).log(Level.SEVERE, null, ex);
            }
        } else {
            System.out.println("File access cancelled by user.");
        }
    }//GEN-LAST:event_jMenuItemOpenActionPerformed

    private void jMenuItemExitActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jMenuItemExitActionPerformed
        System.exit(0);         
    }//GEN-LAST:event_jMenuItemExitActionPerformed

    private void jMenuItemSaveActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jMenuItemSaveActionPerformed
        try{           
            String nombre = "";
            jFileChooser2.showSaveDialog(this);
            File save = jFileChooser2.getSelectedFile();
            if(save != null){
                nombre= jFileChooser2.getSelectedFile().getName();
                FileWriter fw = new FileWriter(save+".txt");                
                fw.write(jTextArea1.getText());                
                fw.close();
            }
        }
        catch(IOException exp){
            System.out.println(exp);
        }
    }//GEN-LAST:event_jMenuItemSaveActionPerformed

    public static void main(String args[]) {
        /* Set the Nimbus look and feel */
        //<editor-fold defaultstate="collapsed" desc=" Look and feel setting code (optional) ">
        /* If Nimbus (introduced in Java SE 6) is not available, stay with the default look and feel.
         * For details see http://download.oracle.com/javase/tutorial/uiswing/lookandfeel/plaf.html 
         */
        try {
            for (javax.swing.UIManager.LookAndFeelInfo info : javax.swing.UIManager.getInstalledLookAndFeels()) {
                if ("Nimbus".equals(info.getName())) {
                    javax.swing.UIManager.setLookAndFeel(info.getClassName());
                    break;
                }
            }
        } catch (ClassNotFoundException ex) {
            java.util.logging.Logger.getLogger(GUIProyectoCO.class.getName()).log(java.util.logging.Level.SEVERE, null, ex);
        } catch (InstantiationException ex) {
            java.util.logging.Logger.getLogger(GUIProyectoCO.class.getName()).log(java.util.logging.Level.SEVERE, null, ex);
        } catch (IllegalAccessException ex) {
            java.util.logging.Logger.getLogger(GUIProyectoCO.class.getName()).log(java.util.logging.Level.SEVERE, null, ex);
        } catch (javax.swing.UnsupportedLookAndFeelException ex) {
            java.util.logging.Logger.getLogger(GUIProyectoCO.class.getName()).log(java.util.logging.Level.SEVERE, null, ex);
        }
        //</editor-fold>

        /* Create and display the form */
        java.awt.EventQueue.invokeLater(new Runnable() {
            public void run() {
                new GUIProyectoCO().setVisible(true);
            }
        });
    }

    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JFileChooser jFileChooser1;
    private javax.swing.JFileChooser jFileChooser2;
    private javax.swing.JMenuBar jMenuBar1;
    private javax.swing.JMenu jMenuFile;
    private javax.swing.JMenuItem jMenuItemExit;
    private javax.swing.JMenuItem jMenuItemOpen;
    private javax.swing.JMenuItem jMenuItemSave;
    private javax.swing.JScrollPane jScrollPane1;
    private javax.swing.JTextArea jTextArea1;
    // End of variables declaration//GEN-END:variables

    //Declaracion de variables propias de la aplicacion
    
    private int K, X, $C;
    private Vector T;
    private Vector Pr;
    private Vector Dmin;
    private Vector Dmax;

    private void Builder(File file) throws FileNotFoundException, IOException{
        FileReader fr = new FileReader(file);
        BufferedReader br = new BufferedReader(fr);
        String line;
        String s;
        StringTokenizer st;        
                
        //Lectura de cantidad de meses
        line = br.readLine();
        K = Integer.parseInt(line);
                
        //Lectura de temperatura
        line = br.readLine();
        st = new StringTokenizer(line,"\t");
        T = new Vector();        
        while(st.hasMoreTokens())
            T.add(st.nextToken());
        
        //Lectura de precipitaciones
        line = br.readLine();
        st = new StringTokenizer(line,"\t");
        Pr = new Vector();        
        while(st.hasMoreTokens())
            Pr.add(st.nextToken());
        
        //Lectura de demanda minima
        line = br.readLine();
        st = new StringTokenizer(line,"\t");
        Dmin = new Vector();        
        while(st.hasMoreTokens())
            Dmin.add(st.nextToken());
                
        //Lectura de demanda maxima
        line = br.readLine();
        st = new StringTokenizer(line,"\t");
        Dmax = new Vector();       
        while(st.hasMoreTokens())
            Dmax.add(st.nextToken());
        
        //Lectura de produccion
        line = br.readLine();
        X = Integer.parseInt(line);

        //Lectura de precio
        line = br.readLine();
        $C = Integer.parseInt(line);
        
        br.close();
        fr.close();
       
    }    
}
