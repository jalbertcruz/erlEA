// 
// Author José Albert Cruz Almaguer <jalbertcruz@gmail.com>
// Copyright 2011 by José Albert Cruz Almaguer.
// 
// This program is licensed to you under the terms of version 3 of the
// GNU Affero General Public License. This program is distributed WITHOUT
// ANY EXPRESS OR IMPLIED WARRANTY, INCLUDING THOSE OF NON-INFRINGEMENT,
// MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. Please refer to the
// AGPL (http://www.gnu.org/licenses/agpl-3.0.txt) for more details.
// 
package manager;

import com.ericsson.otp.erlang.OtpErlangException;
import com.google.gson.Gson;
import config.Data;
import java.awt.Image;
import java.awt.Toolkit;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.rosuda.REngine.REXP;
import org.rosuda.REngine.REXPMismatchException;
import org.rosuda.REngine.Rserve.RConnection;
import org.rosuda.REngine.Rserve.RserveException;

public class MainFrame extends javax.swing.JFrame {

    @SuppressWarnings("CallToThreadStartDuringObjectConstruction")
    public MainFrame() throws OtpErlangException, IOException {
        initComponents();

        Gson g = new Gson();

        FileReader fr = new FileReader(new File("config.json"));

        Data d = g.fromJson(fr, Data.class);
        
        ErlComInterface c = new ErlComInterface(d.getNodeName(), this);
        c.start();
        
    }

    void plotGraph(String data) {
        System.out.println(data);
        try {
            String device = "jpeg"; // device we'll call (this would work with pretty much any bitmap device)

            // connect to Rserve (if the user specified a server at the command line, use it, otherwise connect locally)
            RConnection c = new RConnection();

            // if Cairo is installed, we can get much nicer graphics, so try to load it
            if (c.parseAndEval("suppressWarnings(require('Cairo',quietly=TRUE))").asInteger() > 0) {
                device = "CairoJPEG"; // great, we can use Cairo device
            } else {
                System.out.println("(consider installing Cairo package for better bitmap output)");
            }

            // we are careful here - not all R binaries support jpeg
            // so we rather capture any failures
            REXP xp = c.parseAndEval("try(" + device + "('prueba.jpg',quality=90))");

            if (xp.inherits("try-error")) { // if the result is of the class try-error then there was a problem
                System.err.println("Can't open " + device + " graphics device:\n" + xp.asString());
                // this is analogous to 'warnings', but for us it's sufficient to get just the 1st warning
                REXP w = c.eval("if (exists('last.warning') && length(last.warning)>0) names(last.warning)[1] else 0");
                if (w.isString()) {
                    System.err.println(w.asString());
                }
                return;
            }

            PrintWriter pw = new PrintWriter("result.r");
            // ok, so the device should be fine - let's plot - replace this by any plotting code you desire ...
//            c.parseAndEval("data(iris); attach(iris); plot(Sepal.Length, Petal.Length, col=unclass(Species)); dev.off()");
            c.parseAndEval(data);
            pw.write(data + "\n");
            String data1 = "dt = data.frame(casos, tp)";
            c.parseAndEval(data1);
            pw.write(data1 + "\n");
            String data2 = "boxplot(casos~tp, data=dt, main=\"Example of graph\", xlab=\"Population MaxOnes\", ylab=\"Number of solutions\");";
            c.parseAndEval(data2 + " dev.off()");
            pw.write(data2 + "\n");
            
            pw.close();

            // There is no I/O API in REngine because it's actually more efficient to use R for this
            // we limit the file size to 1MB which should be sufficient and we delete the file as well
//            xp = c.parseAndEval("r=readBin('test.jpg','raw',1024*1024); unlink('test.jpg'); r");
            xp = c.parseAndEval("r=readBin('prueba.jpg','raw',1024*1024); unlink('prueba.jpg'); r");

            // now this is pretty boring AWT stuff - create an image from the data and display it ...
            Image img = Toolkit.getDefaultToolkit().createImage(xp.asBytes());

            // close RConnection, we're done
            c.close();
            if (actual != null) {
                remove(actual);
            }

            actual = new PlotDemo(img);

            add(actual);
            update(getGraphics());
        } catch (RserveException | REXPMismatchException rse) { // RserveException (transport layer - e.g. Rserve is not running)
            System.out.println(rse);
        } catch (Exception e) { // something else
            System.out.println("Something went wrong, but it's not the Rserve: "
                    + e.getMessage());
        }
    }
    PlotDemo actual = null;

    /**
     * This method is called from within the constructor to initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is always
     * regenerated by the Form Editor.
     */
    @SuppressWarnings("unchecked")
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        setDefaultCloseOperation(javax.swing.WindowConstants.EXIT_ON_CLOSE);
        setTitle("Manager");

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(getContentPane());
        getContentPane().setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGap(0, 615, Short.MAX_VALUE)
        );
        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGap(0, 583, Short.MAX_VALUE)
        );

        pack();
    }// </editor-fold>//GEN-END:initComponents

    public void plot(String tp, String casos) {
        plotGraph("tp = " + tp
                + ";casos = " + casos);

    }

    /**
     * @param args the command line arguments
     */
    public static void main(String args[]) {
        /*
         * Set the Nimbus look and feel
         */
        //<editor-fold defaultstate="collapsed" desc=" Look and feel setting code (optional) ">
        /*
         * If Nimbus (introduced in Java SE 6) is not available, stay with the
         * default look and feel. For details see
         * http://download.oracle.com/javase/tutorial/uiswing/lookandfeel/plaf.html
         */
        try {
            for (javax.swing.UIManager.LookAndFeelInfo info : javax.swing.UIManager.getInstalledLookAndFeels()) {
                if ("Nimbus".equals(info.getName())) {
                    javax.swing.UIManager.setLookAndFeel(info.getClassName());
                    break;
                }
            }
        } catch (ClassNotFoundException ex) {
            java.util.logging.Logger.getLogger(MainFrame.class.getName()).log(java.util.logging.Level.SEVERE, null, ex);
        } catch (InstantiationException ex) {
            java.util.logging.Logger.getLogger(MainFrame.class.getName()).log(java.util.logging.Level.SEVERE, null, ex);
        } catch (IllegalAccessException ex) {
            java.util.logging.Logger.getLogger(MainFrame.class.getName()).log(java.util.logging.Level.SEVERE, null, ex);
        } catch (javax.swing.UnsupportedLookAndFeelException ex) {
            java.util.logging.Logger.getLogger(MainFrame.class.getName()).log(java.util.logging.Level.SEVERE, null, ex);
        }
        //</editor-fold>

        /*
         * Create and display the form
         */
        java.awt.EventQueue.invokeLater(new Runnable() {

            @Override
            public void run() {
                try {
                    new MainFrame().setVisible(true);
                } catch (OtpErlangException | IOException ex) {
                    Logger.getLogger(MainFrame.class.getName()).log(Level.SEVERE, null, ex);
                }
            }
        });
    }
    // Variables declaration - do not modify//GEN-BEGIN:variables
    // End of variables declaration//GEN-END:variables
}
